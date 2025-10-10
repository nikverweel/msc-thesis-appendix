##############################
##############################

### Further analysis of Snag methodology for faults
### Nik Verweel
### 16/09/2025

##############################
##############################

source("AHNProcessingFunctions.R")

rsv <- 16
coords <- c("C05", "E15", "H03")

singles <- numeric()
multiples <- numeric()
densities <- numeric()
IntensityIndex <- numeric()

for (coord in coords) {

    lasPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/', coord, '.laz')
    las <- readLAS(lasPath)

    single <- count(las@data[las@data$NumberOfReturns == 1,]) / count(las@data) * 100
    singles <- append(singles, single)

    multi <- count(las@data[las@data$NumberOfReturns > 1,]) / count(las@data) * 100
    multiples <- append(multiples, multi)

    densities <- append(densities, (count(las@data) / 973.14))

    IntensityIndex <- append(IntensityIndex, round((las$Intensity - min(las$Intensity, na.rm = TRUE)) / (max(las$Intensity, na.rm = TRUE) - min(las$Intensity, na.rm = TRUE)) * 255))
    
}

mean(unlist(singles))
mean(unlist(multiples))
mean(unlist(densities))

data_df <- data.frame(IntensityIndex)

p <- ggplot(data_df, aes(x = IntensityIndex)) +
    geom_histogram(aes(y = (..count..) / sum(..count..) * 100), binwidth = 10, boundary = 0, fill = "#31688e", alpha = 0.8, color = "black") +
    labs(
        title = "Intensity histogram summarizing the intensity dynamics \n within the representative sample plots in RSV16",
        x = "Lidar Intensity Index",
        y = "Relative Frequency (% of total)"
    ) + 
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 12),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold", size = 12),
        axis.text = element_text(size = 11),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        axis.ticks = element_line(color = "black")
    ) + 
    scale_x_continuous(
        breaks = c(0, 50, 100, 150, 200, 250),              
        labels = c("0", "50", "100", "150", "200", "250")
    )

ggsave('../Figures/Results/SRO3/intensity_distribution_rsv16.png', p, width = 8, height = 8, dpi = 150)

# Ground filtering for hopefully better results
lasPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/', coord, '.laz')
las <- readLAS(lasPath)
las <- filter_poi(las, Classification != 2)
las@data$IntensityIndex <- round((las$Intensity - min(las$Intensity, na.rm = TRUE)) / (max(las$Intensity, na.rm = TRUE) - min(las$Intensity, na.rm = TRUE)) * 255)

heightThreshold <- 4
pointDensity <- density(las)
maxInt <- max(las$IntensityIndex, na.rm = TRUE)
canopyCover <- (sum(las$Z >= heightThreshold) / npoints(las))
meanCanopyHeight <- (sum(las$Z[las$Z >= heightThreshold]) / sum(las$Z >= heightThreshold))
BBvFr <- ((sum(las$Z >= heightThreshold & (las$IntensityIndex <= 50 | las$IntensityIndex >= 170))) / (sum(las$Z >= heightThreshold & las$IntensityIndex > 50 & las$IntensityIndex < 170)))

### Stage two: Define filtering thresholds ###
LInt_t <- (20 * BBvFr) + (0.075 * maxInt) + 26.5
UInt_t <- (20 * BBvFr) + (0.1875 * maxInt) + 100.25

if (LInt_t < 50) {
LInt_t <- 50
} else if (LInt_t > 70) {
LInt_t <- 70
}

if (UInt_t < 150) {
UInt_t <- 150
} else if (UInt_t > 170) {
UInt_t <- 170
}

### Stage three: Calculate neighbourhood variable statistics ###

# Select overstory points
overstory <- las[las$Z >= heightThreshold]

# Determine the Point Density Requirement (PDR)
pdr <- fcase(
pointDensity < 3, 3,
pointDensity <= 6, 4,
pointDensity <= 12, 5,
pointDensity > 12, 8
)    

# Construct the bbpr matrix
bbpr_thresholds <- matrix(c(0.95, 0.95, 0.725,
                            0.90, 0.90, 0.65,
                            0.85, 0.90, 0.70,
                            0.95, 0.95, 0.55), 
                            nrow = 3, ncol = 4)

# Perform the snag point detection using the Wing 2015 algorithm
snags <- segment_snags(overstory, algorithm = wing2015(low_int_thrsh = LInt_t, uppr_int_thrsh = UInt_t, pt_den_req = pdr, BBPRthrsh_mat = bbpr_thresholds))
snags <- filter_poi(snags, snagCls > 0)

count(snags@data) / count(las@data) * 100

# Plotting intensity plot after filtering ground points
las_intensity <- as.data.frame(las@data$IntensityIndex)

p <- ggplot(las_intensity, aes(x = las@data$IntensityIndex)) +
    geom_histogram(aes(y = (..count..) / sum(..count..) * 100), binwidth = 10, boundary = 0, fill = "#31688e", alpha = 0.8, color = "black") +
    labs(
        title = "Intensity histogram summarizing the intensity dynamics \n in ground-filtered sample plot S09 in RSV 3",
        x = "Lidar Intensity Index",
        y = "Relative Frequency (% of total)"
    ) + 
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 12),
        axis.title.y = element_text(margin = margin(r = 10), face = "bold", size = 12),
        axis.text = element_text(size = 11),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
        axis.ticks = element_line(color = "black")
    ) + 
    scale_x_continuous(
        breaks = c(0, 50, 100, 150, 200, 250),              
        labels = c("0", "50", "100", "150", "200", "250")
    )

ggsave('../Figures/Results/SRO3/intensity_distribution_filtered.png', p, width = 8, height = 8, dpi = 150)


# Resampling the point cloud after ground class removal
rsvs <- c(3, 16, 19)
densities <- numeric()
pers <- numeric()

for (rsv in rsvs) {

    if (rsv == 3) {
        coords <- c("A04", "G05", "S09")
    } else if (rsv == 16) {
        coords <- c("C05", "E15", "H03")
    } else {
        coords <- c("D02", "G01", "J05")
    }

    for (coord in coords) {

        lasPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/', coord, '.laz')

        las <- readLAS(lasPath)
        las <- filter_poi(las, Classification != 2)
        
        low_las <- decimate_points(las, algorithm = random(6.8))
        low_las@data$IntensityIndex <- round((low_las$Intensity - min(low_las$Intensity, na.rm = TRUE)) / (max(low_las$Intensity, na.rm = TRUE) - min(low_las$Intensity, na.rm = TRUE)) * 255)

        heightThreshold <- 4
        pointDensity <- density(low_las)
        maxInt <- max(low_las$IntensityIndex, na.rm = TRUE)
        canopyCover <- (sum(low_las$Z >= heightThreshold) / npoints(low_las))
        meanCanopyHeight <- (sum(low_las$Z[low_las$Z >= heightThreshold]) / sum(low_las$Z >= heightThreshold))
        BBvFr <- ((sum(low_las$Z >= heightThreshold & (low_las$IntensityIndex <= 50 | low_las$IntensityIndex >= 170))) / (sum(low_las$Z >= heightThreshold & low_las$IntensityIndex > 50 & low_las$IntensityIndex < 170)))

        LInt_t <- (20 * BBvFr) + (0.075 * maxInt) + 26.5
        UInt_t <- (20 * BBvFr) + (0.1875 * maxInt) + 100.25

        if (LInt_t < 50) {
        LInt_t <- 50
        } else if (LInt_t > 70) {
        LInt_t <- 70
        }

        if (UInt_t < 150) {
        UInt_t <- 150
        } else if (UInt_t > 170) {
        UInt_t <- 170
        }

        overstory <- las[las$Z >= heightThreshold]

        pdr <- fcase(
        pointDensity < 3, 3,
        pointDensity <= 6, 4,
        pointDensity <= 12, 5,
        pointDensity > 12, 8
        )

        bbpr_thresholds <- matrix(c(0.95, 0.95, 0.725,
                                    0.90, 0.90, 0.65,
                                    0.85, 0.90, 0.70,
                                    0.95, 0.95, 0.55), 
                                    nrow = 3, ncol = 4)

        snags <- segment_snags(overstory, algorithm = wing2015(low_int_thrsh = LInt_t, uppr_int_thrsh = UInt_t, pt_den_req = pdr, BBPRthrsh_mat = bbpr_thresholds))
        snags <- filter_poi(snags, snagCls > 0)

        per <- count(snags@data) / count(las@data) * 100
        print(paste0("RSV ", rsv, " COORD ", coord, ": ", per, "% - Density:", pointDensity))

        pers <- append(pers, per)
        densities <- append(densities, pointDensity)

    }

}

mean(unlist(pers))
mean(tail(unlist(pers), 3))

