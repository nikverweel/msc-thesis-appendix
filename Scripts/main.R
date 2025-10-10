##############################
##############################

### Main
### Nik Verweel
### 27/03/2025

##############################
##############################

# Source functions
source('funcs.R')

# Preprocessing - subsetting for SRO2
preprocessingResults <- preproccesing(subset = TRUE, sublist = c(3, 16, 19))

# Unpack results from preprocessing 
fr_boundaries <- preprocessingResults[[1]]
fr_cores <- preprocessingResults[[2]]
fr_samples <- preprocessingResults[[3]]
rm(preprocessingResults)

# Plot the preprocessed (and subsetted) reserves
plot_fr(fr_boundaries, fr_cores, fr_samples, labelling = TRUE, saveImage = TRUE, '../Figures/subset_forest_reserves.png')

# Download data for the subsetted areas
downloadData(fr_boundaries, AHNversion = 'AHN4', overrideDownload = FALSE)

# Clip the data
clipData(fr_boundaries, fr_cores, fr_samples, overwriteClip = TRUE)

# Calculate most frequent tree types
frequentTrees(fr_boundaries, fr_cores, fr_samples, level = 'all')

# Create Rasters (DTM, DSM, CHM)
createRasters(fr_boundaries, resolution = 0.5, overwriteRaster = TRUE)

# Perform ITD for the SRO2 subset
individualTreeDetection(fr_samples = fr_samples[fr_samples$rsv_code == 3,], 
                        subset = TRUE, 
                        subSamples = c("A04", "G05", "S09"),
                        overwriteITD = TRUE, saveImg = TRUE)
individualTreeDetection(fr_samples = fr_samples[fr_samples$rsv_code == 16,], 
                        subset = TRUE, 
                        subSamples = c("C05", "E15", "H03"),
                        overwriteITD = TRUE, saveImg = TRUE)
individualTreeDetection(fr_samples = fr_samples[fr_samples$rsv_code == 19,], 
                        subset = TRUE, 
                        subSamples = c("D02", "G01", "J05"),
                        overwriteITD = TRUE, saveImg = TRUE)

# Perform canopy projection analysis for the SRO2 subset
canopyProjection(fr_samples = fr_samples[fr_samples$rsv_code == 3,], 
                 subset = TRUE, 
                 subSamples = c("A04", "G05", "S09"),
                 overwriteCP = TRUE, saveImg = TRUE)
canopyProjection(fr_samples = fr_samples[fr_samples$rsv_code == 16,], 
                 subset = TRUE,
                 subSamples = c("C05", "E15", "H03"),
                 overwriteCP = TRUE, saveImg = TRUE)
canopyProjection(fr_samples = fr_samples[fr_samples$rsv_code == 19,], 
                 subset = TRUE,
                 subSamples = c("D02", "G01", "J05"),
                 overwriteCP = TRUE, saveImg = TRUE)

# Perform snag detection for the SRO2 subset
snagDetection(fr_samples = fr_samples[fr_samples$rsv_code == 3,], 
              subset = TRUE, 
              subSamples = c("A04", "G05", "S09"), heightThreshold = 2)
snagDetection(fr_samples = fr_samples[fr_samples$rsv_code == 16,], 
              subset = TRUE,
              subSamples = c("C05", "E15", "H03"), heightThreshold = 2)
snagDetection(fr_samples = fr_samples[fr_samples$rsv_code == 19,], 
              subset = TRUE,
              subSamples = c("D02", "G01", "J05"), heightThreshold = 2)

### SRO3 - ITD finetuning and comparison ###
preprocessFielddata()
addLocationData()

individualTreeDetection(fr_samples, 
                        subset = FALSE, 
                        overwriteITD = TRUE, 
                        saveImg = FALSE)

accuracyAnalysis(overwriteAccuracy = TRUE)


data <- read.csv("D:/Git repositories/msc-thesis/Data/FrequentTrees/FrequentTreesPerRSV.csv")
nrow(data[data$forestType == "Coniferous",])
