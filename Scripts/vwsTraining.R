##############################
##############################

### VWS Training
### Nik Verweel
### 21/08/2025

##############################
##############################

# User inputs: define RSV and coord for which to perform the analysis
rsv <- 19
coord <- "J05"
chm <- rast(paste0('../Data/AHN/RSV_', rsv, '/Rasters/', coord, '_CHM_0_5m.tif'))
treeNum <- 10

# Load in the forest types to determine what forest type this is
freqTreesPath <- paste0('../Data/FrequentTrees/RSV_', rsv, '/PlotFrequentTrees.csv')
freqTreesData <- read.csv(freqTreesPath)
forestTypePlot <- freqTreesData[freqTreesData$RUIT_COORD == coord,]$forestType

# Check if save location exists
saveDir <- '../Data/VWS/'
if (!dir.exists(saveDir)) {
    dir.create(saveDir)
}

# Data frame for saving results
results <- data.frame(height = numeric(treeNum), radius = numeric(treeNum))

# Actual training process
windows()
plot(chm, col=viridis(100))
for (i in 1:treeNum) {

    message(paste("Measuring tree", i, "of", treeNum))

    message("Click on the treetop")
    treetop_xy <- click(chm, n = 1, xy = TRUE, type = 'p', col = 'red', pch = 3)

    message("Click on the tree edge")
    edge_xy <- click(chm, n = 1, xy = TRUE, type = 'p', col = 'cyan', pch = 3)

    coords_df <- data.frame(x = treetop_xy$x, y = treetop_xy$y)
    height <- terra::extract(chm, coords_df)[,2]
    radius <- sqrt((treetop_xy$x - edge_xy$x)^2 + (treetop_xy$y - edge_xy$y)^2)

    results$height[i] <- height
    results$radius[i] <- radius

}
dev.off()

# Save the results
savePath <- paste0(saveDir, forestTypePlot, '.csv')
write.table(results, savePath, row.names = FALSE, append = TRUE, col.names = FALSE, sep=',')

### Build the models ###

# Mixed Forests
mixedData <- read.csv('../Data/VWS/Mixed.csv', header=FALSE)
names(mixedData) <- c("Height", "Radius")
mixedModel <- lm(Radius ~ Height, data=mixedData)

plot(Radius ~ Height, data = mixedData,
     main = "Tree Height vs. Crown Radius - Mixed Forests",
     xlab = "Tree height (m)",
     ylab = "Crown radius (m)",
     pch = 16)
abline(mixedModel, col='red', lwd=2)
summary(mixedModel)

# Deciduous Forests
deciduousData <- read.csv('../Data/VWS/Deciduous.csv', header=FALSE)
names(deciduousData) <- c("Height", "Radius")
deciduousModel <- lm(Radius ~ Height, data=deciduousData)

plot(Radius ~ Height, data = deciduousData,
     main = "Tree Height vs. Crown Radius - Mixed Forests",
     xlab = "Tree height (m)",
     ylab = "Crown radius (m)",
     pch = 16)
abline(deciduousModel, col='red', lwd=2)
summary(deciduousModel)

# Coniferous Forests
coniferousData <- read.csv('../Data/VWS/Coniferous.csv', header=FALSE)
names(coniferousData) <- c("Height", "Radius")
coniferousModel <- lm(Radius ~ Height, data=coniferousData)

plot(Radius ~ Height, data = coniferousData,
     main = "Tree Height vs. Crown Radius - Mixed Forests",
     xlab = "Tree height (m)",
     ylab = "Crown radius (m)",
     pch = 16)
abline(coniferousModel, col='red', lwd=2)
summary(coniferousModel)
