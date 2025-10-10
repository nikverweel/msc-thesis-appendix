##############################
##############################

### AHN Functions
### Corné Vreugdenhil
### 04/09/2024

##############################
##############################

## Function that checks if package exists and installs if necessary
CheckPackages <- function(packagelist){
  #Function which takes a package name, then checks if it's installed and if not, installs it.
  for (packagename in packagelist){
    if(!require(packagename, character.only = TRUE)){
      install.packages(packagename, character.only = TRUE)
      require(packagename, character.only = TRUE)
    }
    library(packagename, character.only = TRUE)
  }
}

## Function that checks which AHN tiles are needed for your extent
# note that the bbox is expected to be within the Netherlands and in the Dutch projection system RDnew (EPSG:28992)
CheckAHNtiles <- function(bbox=ext(174000, 174100, 473000, 473100), AHNfolder="data/AHN", Tilingscheme="AHN"){
  workdir <- paste(getwd(), "/", AHNfolder, "/AHN_Tilingscheme", sep="")
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  MainWorkdir <- getwd()
  setwd(workdir)
  AHNtilesfile <- "AHN_TilingSchemes.gpkg"
  if(!file.exists(AHNtilesfile)){
    #AHNtiles <- st_read('https://static.fwrite.org/2023/01/AHN_AHN_GeoTiles.zip') # based on https://twiav.nl/nl/WFS_R.php
    download.file('https://static.fwrite.org/2022/01/index_sheets.gpkg_.zip', 
                  "AHNtilescheme.zip", method="auto", mode='wb')
    unzip("AHNtilescheme.zip", exdir='.')
    file.rename("index_sheets.gpkg", AHNtilesfile)
  }
  AHNtiles <- vect(AHNtilesfile, layer = Tilingscheme)
  setwd(MainWorkdir)
  tiles <- crop(AHNtiles, bbox)
  return(array(tiles$AHN))
}
CheckGeotiles <- function(bbox=ext(174000, 174100, 473000, 473100), AHNfolder="data/AHN"){
  return(CheckAHNtiles(bbox=bbox, AHNfolder = AHNfolder, Tilingscheme = "AHN_subunits"))
}

## Function for downloading AHN data
DownloadAHN <- function(AHNtile = '32fn1', outputfolder = 'data/AHN/OriginalAHNdata', AHNversion = 'AHN2', resolution = '5m', modelversion = 'DTM'){
  
  # create downloads folder if it does not exist yet
  if(!file.exists(paste(getwd(), "/", outputfolder, sep=""))){
    dir.create(outputfolder, recursive = TRUE)
  }
  
  #set up download url
  downloadurl <- 'nothing'

  # below links can get outdated!! see https://ahn.arcgisonline.nl/ahnviewer/ and downloadsheets to find out the current link format
  
  if(AHNversion == 'AHN2' & resolution == '5m' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN2/DTM_5m/ahn2_5_', tolower(AHNtile), '.tif.zip', sep='')}
  if(AHNversion == 'AHN2' & resolution == '50cm' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN2/DTM_50cm/i', tolower(AHNtile), '.tif.zip', sep='')}
  if(AHNversion == 'AHN2' & resolution == '50cm' & modelversion == 'DSM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN2/DSM_50cm/r', tolower(AHNtile), '.tif.zip', sep='')}
  if(AHNversion == 'AHN2' & resolution == '50cm' & modelversion == 'DSM'){
    downloadurl <- paste('http://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/r', tolower(AHNtile), '.tif.zip', sep='')}
  if(AHNversion == 'AHN2' & resolution == 'pointcloud' & modelversion == 'DSM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN2/ahn2_laz_units_uitgefilterd/u', tolower(AHNtile), '.laz.zip', sep='')}
  if(AHNversion == 'AHN2' & resolution == 'pointcloud' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN2/ahn2_laz_units_gefilterd/g', tolower(AHNtile), '.laz.zip', sep='')}
  
  if(AHNversion == 'AHN3' & resolution == '5m' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN3/DTM_5m/M5_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN3' & resolution == '5m' & modelversion == 'DSM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN3/DSM_5m/R5_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN3' & resolution == '50cm' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN3/DTM_50cm/M_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN3' & resolution == '50cm' & modelversion == 'DSM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN3/DSM_50cm/R_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN3' & resolution == 'pointcloud' & modelversion == 'full'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/AHN3/LAZ/C_', toupper(AHNtile), '.LAZ', sep='')}

    if(AHNversion == 'AHN4' & resolution == '5m' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/ahn4/02b_DTM_5m/M5_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN4' & resolution == '5m' & modelversion == 'DSM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/ahn4/03b_DSM_5m/R5_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN4' & resolution == '50cm' & modelversion == 'DTM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/ahn4/02a_DTM_0.5m/M_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN4' & resolution == '50cm' & modelversion == 'DSM'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/ahn4/03a_DSM_0.5m/R_', toupper(AHNtile), '.zip', sep='')}
  if(AHNversion == 'AHN4' & resolution == 'pointcloud' & modelversion == 'full'){
    downloadurl <- paste('https://ns_hwh.fundaments.nl/hwh-ahn/ahn4/01_LAZ/C_', toupper(AHNtile), '.LAZ', sep='')}
  
  #really download the tile
  outputfilename <- paste(AHNversion, '_', resolution, '_', modelversion, '_', AHNtile, sep='')
  if(downloadurl == 'nothing'){
    stop('try again and check your input parameters!')
  }
  if(length(list.files(path = outputfolder, pattern = outputfilename)) > 0){
    print('The requested AHN tile already is available locally in the given folder')
    return('The requested AHN tile already is available locally in the given folder')
  } 
  
  print('Okay we are going to download...')
  
  outputfilenamefull <- paste(outputfolder, '/', outputfilename, sep='')
  
  if(resolution == 'pointcloud' & AHNversion %in% c('AHN3', 'AHN4')){
    outputfilenamefull <- paste(outputfilenamefull, ".LAZ", sep="")
    download.file(downloadurl, outputfilenamefull, method="auto", mode='wb')
  } else if(resolution == 'pointcloud' & AHNversion %in% c('AHN2')){
    zipname <- paste(outputfilenamefull, '.zip', sep='')
    download.file(downloadurl, zipname, method="auto", mode='wb')
    tempfolder = paste('./', outputfolder, '/temp', sep='')
    unzip(zipname, exdir=tempfolder)
    file.rename(list.files(tempfolder, full.names = TRUE)[1], paste(outputfilenamefull, '.LAZ', sep=''))
    unlink(tempfolder, recursive = TRUE)
    unlink(zipname, recursive = TRUE)
  } else {
    zipname <- paste(outputfilenamefull, '.zip', sep='')
    download.file(downloadurl, zipname, method="auto", mode='wb')
    tempfolder = paste('./', outputfolder, '/temp', sep='')
    unzip(zipname, exdir=tempfolder)
    file.rename(list.files(tempfolder, full.names = TRUE)[1], paste(outputfilenamefull, '.tif', sep=''))
    unlink(tempfolder, recursive = TRUE)
    unlink(zipname, recursive = TRUE)
  }
}

DownloadGeotiledAHN <- function(geotilecode="26HZ1_16", AHNversion="AHN2", outputfilepath="AHNdata/Geotiles"){
  # create downloads folder if it does not exist yet
  if(!file.exists(paste(getwd(), "/", outputfilepath, sep=""))){
    dir.create(outputfilepath, recursive = TRUE)
  }
  if(!file.exists(paste(getwd(), "/", outputfilepath, "/", AHNversion, "_", geotilecode, ".LAZ", sep=""))){
    download.file(paste("https://geotiles.citg.tudelft.nl/", AHNversion, "_T/", geotilecode, ".LAZ", sep=""), paste(outputfilepath, "/", AHNversion, "_", geotilecode, ".LAZ", sep=""), method="auto", mode='wb')
    download.file(paste("https://geotiles.citg.tudelft.nl/", AHNversion, "_T/", geotilecode, ".LAX", sep=""), paste(outputfilepath, "/", AHNversion, "_", geotilecode, ".LAX", sep=""), method="auto", mode='wb')
    download.file(paste("https://geotiles.citg.tudelft.nl/", AHNversion, "_T/", geotilecode, ".txt", sep=""), paste(outputfilepath, "/", AHNversion, "_", geotilecode, ".txt", sep=""), method="auto", mode='wb')
  }
  else{print(paste('Geotile ', geotilecode, ' was already downloaded.',sep=""))}
}

# Function to mosaic AHN rasters and clip to area of interest
CropAndMosaicRasters <- function(patt, ext, origdatafolder, outputfolder){
  rastfileslist <- list.files(origdatafolder, pattern = patt, full.names = TRUE) # list files that belong to each other
  if(length(rastfileslist) == 0){
    print("No file found for given pattern!")
  }
  if(length(rastfileslist) == 1){
    rasty <- rast(rastfileslist)
    rasty <- crop(rasty, ext) # crop mosaic of raster to the AOI extent
    names(rasty) <- c(patt) 
    writeRaster(rasty, paste(outputfolder, '/', patt, '.tif', sep="")) # save as new raster
    return(rasty)
  }
  if(length(rastfileslist) >= 2){
    rasty <- sprc(rastfileslist) # read as spatrastercollection (easy way to read in list of files with rast)
    rasty <- mosaic(rasty) # mosaic together, knowing that they are not overlapping
    rasty <- crop(rasty, ext) # crop mosaic of raster to the AOI extent
    names(rasty) <- c(patt) 
    writeRaster(rasty, paste(outputfolder, '/', patt, '.tif', sep="")) # save as new raster
    return(rasty)
  }
}

## function that processes a Lidar PC and derives tree tops and their heights and dominance
ExtractDominantTrees <- function(PC, fulloutput=FALSE){
  dtm = rasterize_terrain(PC, res = 0.5, algorithm = tin(), keep_lowest = TRUE, full_raster = TRUE)
  dsm = pixel_metrics(PC, ~max(Z), 0.5)
  
  chm = dsm - dtm 
  
  treetops <- locate_trees(PC, lmf(ws=5, hmin=2, shape="circular"))
  treetops <- crop(vect(treetops), ext(PC)) # useful for catalog processing: remove points outside the buffer
  
  treetops$chm <- extract(chm, ID=FALSE, treetops, fun=max, method="simple")
  treetops$chm_5m <- extract(chm, ID=FALSE, buffer(treetops, 10), fun=max, method="simple")
  treetops$dominance <- ifelse(treetops$chm_5m > treetops$chm, 2, 1)
  
  ifelse(fulloutput == TRUE,
         yes = return(list(treetops, dtm, dsm, chm)), 
         no = return(treetops))
}  

## function that processes the lidar PC in parallel processing using the catalog function
CatalogProcessPC <- function(PCchunk){
  las <- readLAS(PCchunk)               # read the chunk
  if (is.empty(las)) return(NULL)     # exit if empty
  
  trees = ExtractDominantTrees(las, fulloutput = FALSE)
  
  return(trees)
}
