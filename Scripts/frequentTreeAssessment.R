##############################
##############################

### Assessment of frequency of trees
### Nik Verweel
### 02/10/2025

##############################
##############################

frequentTrees <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
frequentTrees <- frequentTrees[!is.na(frequentTrees$forestType),]

trees <- frequentTrees[frequentTrees$forestType == "Deciduous",]
count(frequentTrees[frequentTrees$forestType == "Deciduous",])

totalCon <- 0
totalMix <- 0
totalDec <- 0

for (i in 1:nrow(trees)) {
    
    rsv <- trees$RSV_CODE[i]
    forest <- trees$forestType[i]

    filePath <- paste0('../Data/FrequentTrees/RSV_', rsv, '/PlotFrequentTrees.csv')
    plotTypes <- read.csv(filePath)
    plotTypes <- plotTypes[!is.na(plotTypes$forestType),]
    
    totalCon <- totalCon + count(plotTypes[plotTypes$forestType == "Coniferous",])
    totalMix <- totalMix + count(plotTypes[plotTypes$forestType == "Mixed",])
    totalDec <- totalDec + count(plotTypes[plotTypes$forestType == "Deciduous",])

}

print(paste0("CON: ", totalCon))
print(paste0("MIX: ", totalMix))
print(paste0("DEC: ", totalDec))

