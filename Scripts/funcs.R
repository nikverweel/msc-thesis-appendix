##############################
##############################

### Funcs
### Nik Verweel
### 12/03/2025

##############################
##############################

# Import functions
source("AHNProcessingFunctions.R")

# Check required packages
CheckPackages(c('sf', 'dplyr', 'terra', 'lidR', 'rgl', 'stringr', 'ggplot2', 'readxl',
'ggspatial', 'patchwork', 'viridis', 'ForestTools', 'tidyr', 'ggnewscale', 'RANN', 'data.table',
'colorspace', 'RColorBrewer'))

# Function for preprocessing forest reserve data
preproccesing <- function(subset = FALSE, sublist = NULL) {
  
  # Error handling
  if (subset && is.null(sublist)) {
    stop('Sublist must be providede when subset = TRUE.')
  }
  
  # Load in the Forest Reserve boundaries
  fr_boundaries <- st_read("../Data/GIS/ReserveBoundary.shp")
  fr_boundaries <- fr_boundaries[(fr_boundaries$RSVCODE != 55),]    # Removal of excluded Forest Reserve (not yet processed in Database)
  fr_boundaries <- st_set_crs(fr_boundaries, 7415)                  # RD New + NAP Height
  
  # Load in the Core Areas and Sample Sites and filter them
  fr_cores <- st_read("../Data/GIS/ReserveCoreaAreaPolygon.shp") # nolint
  fr_cores <- fr_cores[((fr_cores$rsvcode != 55) & (fr_cores$rsvcode <=  60)),]
  fr_cores <- st_set_crs(fr_cores, 7415)
  
  fr_samples <- st_read("../Data/GIS/ReserveSamplePlots.shp")
  fr_samples <- fr_samples[fr_samples$rsv_code != 55,]
  fr_samples <- fr_samples[is.na(fr_samples$opmerking), ]           # Remove the samples that fall outside of the reserve boundaries
  fr_samples <- st_set_crs(fr_samples, 7415)
  
  # Dissolving the boundary geometries
  fr_boundaries <- fr_boundaries %>%
    group_by(RSVCODE, NAAM) %>%
    summarise(geometry = st_union(geometry))
  
  # Subset the data (optional)
  if (subset) {
    fr_boundaries <- fr_boundaries[fr_boundaries$RSVCODE %in% sublist, ]
    fr_cores <- fr_cores[fr_cores$rsvcode %in% sublist, ]
    fr_samples <- fr_samples[fr_samples$rsv_code %in% sublist, ]
  }
  
  return(list(fr_boundaries, fr_cores, fr_samples))
  
}

# Automatic plot creation for reserves
plot_fr <- function(fr_boundaries, fr_cores, fr_samples, labelling = TRUE, saveImage = FALSE, fileName = NULL) {
  
  # Error handling
  if (!inherits(fr_boundaries, "sf")) {
    stop("fr_boundaries must be an sf object.")
  }
  if (!inherits(fr_cores, "sf")) {
    stop("fr_cores must be an sf object.")
  }
  if (!inherits(fr_samples, "sf")) {
    stop("fr_samples must be an sf object.")
  }
  if (saveImage && is.null(fileName)) {
    stop("savePath must be specified if saveImage is set to TRUE.")
  }
  if (labelling && (nrow(fr_boundaries) > 1)) {
    warning("Labels are only generated for single plots, skipping due to n > 1.")
  }
  
  # Obtain the unique RSV codes for plotting purposes
  rsv_codes <- unique(fr_boundaries$RSVCODE)
  
  # Create empty list to place plots in
  plots <- list()
  
  for (code in rsv_codes) {
    
    # Subset for current RSV Code
    b_sub <- fr_boundaries[fr_boundaries$RSVCODE == code, ]
    c_sub <- fr_cores[fr_cores$rsvcode == code, ]
    s_sub <- fr_samples[fr_samples$rsv_code == code, ]
    
    # Calculate bounding box to ensure square plotting window
    bbox <- st_bbox(b_sub)
    max_dim <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
    center_x <- (bbox["xmin"] + bbox["xmax"]) / 2
    center_y <- (bbox["ymin"] + bbox["ymax"]) / 2
    half_dim <- max_dim / 2
    sq_xmin <- center_x - (half_dim + 50)
    sq_ymin <- center_y - (half_dim + 50)
    sq_xmax <- center_x + (half_dim + 50)
    sq_ymax <- center_y + (half_dim + 50)

    # Manual highlighting for visualisation purposes
      # highlight <- s_sub[s_sub$RUIT_COORD %in% c("A04", "G05", "S09"),]
      # highlight <- s_sub[s_sub$RUIT_COORD %in% c("C05", "E15", "H03"),]
      highlight <- s_sub[s_sub$RUIT_COORD %in% c("D02", "G01", "J05"),]

    # Plotting
    p <- ggplot() +
      geom_sf(data = b_sub, fill = "gray90", color = "black", linewidth = 0.7) +
      geom_sf(data = c_sub, fill = NA, color = "red", linewidth = 0.5) +
      geom_sf(data = s_sub, shape = 3, color = "black", size = 0.8) + 
      geom_sf(data = highlight, shape = 3, color = "#ac0000", size = 2) + 
      annotation_scale(
        location = "bl", 
        width_hint = 0.2,
        text_cex = 0.8,
        text_family = "CMSS",
        line_width = 0.8
      ) +
      annotation_north_arrow(
        location = "tl", 
        which_north = "true",
        style = north_arrow_fancy_orienteering(
          line_width = 0.8,
          text_size = 6,
        ),
        height = unit(1, "cm"),
        width = unit(1, "cm")
      ) +  
      labs(title = paste("Forest Reserve:", code, b_sub$NAAM)) +
      theme_minimal(base_size = 6) +
      theme(
        plot.title = element_text(
          size = 10, 
          face = "bold"
        )
      ) +
      coord_sf(
        xlim = c(sq_xmin, sq_xmax),
        ylim = c(sq_ymin, sq_ymax),
        expand = FALSE,
        crs = crs(b_sub)
      ) +
      coord_sf(crs = st_crs(28992), datum = st_crs(28992))
    
    # Add labels if only one plot
    if (labelling && length(rsv_codes) == 1) {
      p <- p + geom_text(
        data = s_sub,
        aes(x = st_coordinates(s_sub)[, 1], y = st_coordinates(s_sub)[, 2], label = RUIT_COORD),
        color = "gray30", size = 2.5, vjust = -1
      )
      
    }
    
    # Store plot in list
    plots[[as.character(code)]] <- p
    
  }
  
  # Determine the rows / columns
  n_cols <- min(length(rsv_codes), 3)
  n_rows <- ceiling(length(rsv_codes) / n_cols)
  
  # Wrap the plots
  wrap_plots(plots, ncol = n_cols, nrow = n_rows)
  
  # Saving the image if requested
  if (saveImage) {
    ggsave(fileName, width = (n_cols * 2000), height = (n_rows * 2000), units = "px", dpi = 350)
  }
  
}

# Retrieves geotiles and downloads data
downloadData <- function(fr_boundaries = NULL, timeout = 3000, AHNversion, overrideSize = FALSE, overrideDownload = FALSE) {
  
  # Error handling
  if (is.null(fr_boundaries)) {
    stop("Error, must provide forest reserve boundaries.")
  }
  if (!inherits(fr_boundaries, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  if (nrow(fr_boundaries) > 5 && !overrideSize) {
    stop("WARNING - Downloading a vast amount of data! If you're sure, set overrideSize to TRUE")
  }
  
  options(timeout = timeout)
  
  # Download the data per reserve
  for (i in 1:nrow(fr_boundaries)) {
    
    # Get RSV code
    rsv <- fr_boundaries[i, ]$RSVCODE
    
    # Create file name
    filePath <- paste0('../Data/AHN/RSV_', rsv)

    # Create directory to save the data
    if (!file.exists(filePath)) {
      dir.create(filePath)
    } else {
      warning(paste("Directory", filePath, "already exists."))
    }
    
    # Construction geotile file path
    tileFilePath <- paste0(filePath, '/geotiles.csv')
    
    # Check if geotile information has been previously collected
    if (file.exists(tileFilePath) && !overrideDownload) {
      # Notify that there's already a file with geotiles
      print(paste0('Geotiles already identified for RSV_', rsv,  ', checking download status...'))
      
      # Read the existing file with geotiles information
      tiles <- read.csv(tileFilePath, header = TRUE)
      
    } else {
      # Notify that geotiles are being collected
      print(paste0('Collection geotile information for RSV_', rsv))
      tiles <- CheckGeotiles(bbox = st_bbox(fr_boundaries[i, ]), AHNfolder = '../Data/AHN')
      tiles <- as.data.frame(tiles)
     
      # Save the tiles to a file
      write.csv(tiles, file = tileFilePath, row.names = FALSE)

    }

    # Construct path for data download
    cloudFilePath <- paste0(filePath, '/LAZ/')
    
    # Delete data if override is requested
    if (overrideDownload) {
      unlink(list.files(cloudFilePath, full.names = TRUE))
    }
    
    # Check if there are already clipped versions available
    if ((length(list.files(path = paste0(filePath, '/Clipped'), pattern = '*.laz')) > 0) && !overrideDownload) {
      warning('WARNING - Clipped point clouds already detected, are you sure you wish to download the data again? Please set overrideDownload to TRUE.')
    } else {
      # Download the data using the geotiles
      for (i in 1:nrow(tiles)) {
        DownloadGeotiledAHN(geotilecode = tiles[i, ], AHNversion = AHNversion, outputfilepath = cloudFilePath)
      }
    }

  }
  
}

# Clips AHN data to size of sample plots
clipData <- function(fr_boundaries = NULL, fr_cores = NULL, fr_samples = NULL, overwriteClip = FALSE, deleteSource = FALSE) {
  
  # Error handling
  if (is.null(fr_boundaries)) {
    stop("Error, must provide forest reserve boundaries.")
  }
  if (!inherits(fr_boundaries, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  if (is.null(fr_cores)) {
    stop("Error, must provide forest reserve core areas")
  }
  if (!inherits(fr_cores, 'sf')) {
    stop("fr_cores must be an sf object.")
  }
  if (is.null(fr_samples)) {
    stop("Error, must provide forest reserve sample plots")
  }
  if (!inherits(fr_samples, 'sf')) {
    stop("fr_samples must be an sf object.")
  }
  
  # Iterate through the forest reserves
  for (i in 1:nrow(fr_boundaries)) {
    
    # Get RSV Code
    rsv <- fr_boundaries[i, ]$RSVCODE
    print(paste0('Now processing RSV ', rsv))
    
    # Constuct LAS path
    lasPath <- paste0('../Data/AHN/RSV_', rsv, '/LAZ/')
    
    # Create directory if it isn't available yet
    clippedPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/')
    if (!file.exists(clippedPath)) {
      dir.create(clippedPath)
    }
    
    # Clipping the core area (if it doesn't yet exist)
    outPath <- paste0(clippedPath, 'core')
    if (file.exists(paste0(outPath, '.laz')) && !overwriteClip) {
      print('Clip for core area already exists. Skipping.')
    } else {
      # Check if the path exists and contains files
      if (length(list.files(path = lasPath, pattern = '*.LAZ')) == 0) {
        stop(paste0('Data for RSV_', rsv, ' not found. Please download data first!'))
      }
      
      # Create LAS Catalog
      ctg <- suppressWarnings(readALSLAScatalog(lasPath))
      
      # Checking the Catalog
      las_check(ctg)
      
      # Save the clipped file
      opt_output_files(ctg) <- outPath
      opt_laz_compression(ctg) <- TRUE
      roi <- clip_roi(ctg, st_buffer(fr_cores[fr_cores$rsvcode == rsv, ], 5))
      
      # Filter duplicates
      ctg <- readALSLAScatalog(paste0(outPath, '.laz'))
      opt_output_files(ctg) <- outPath
      opt_laz_compression(ctg) <- TRUE
      filtered <- filter_duplicates(ctg)
      
    }
    
    # Get the samples for this RSV
    fr_samples_sub <- fr_samples[fr_samples$rsv_code == rsv, ]
    
    # Get coordinates for the sample sites
    sampleCoordinates <- st_coordinates(fr_samples_sub)
    
    # Clipping all the sample sites (if it doesn't yet exist)
    for (i in 1:nrow(fr_samples_sub)) {
      
      # Create file path to save the clipped circle to
      outPath <- paste0(clippedPath, fr_samples_sub[i, ]$RUIT_COORD)
      
      # Check if the file doesn't exist yet
      if (file.exists(paste0(outPath, '.laz')) && !overwriteClip) {
        print(paste0('Clip for sample site ', fr_samples_sub[i, ]$RUIT_COORD, ' already exists. Skipping.'))
      } else {
        # Reset the LAS catalog
        ctg <- suppressWarnings(readALSLAScatalog(lasPath))
        
        # Actually clip the sample site
        opt_output_files(ctg) <- outPath
        opt_laz_compression(ctg) <- TRUE
        rois <- clip_circle(ctg, sampleCoordinates[i, 'X'], sampleCoordinates[i, 'Y'], 17.6)
        
        # Filter duplicates if the clip exists (i.e., no empty cloud)
        if (file.exists(paste0(outPath, '.laz'))) {
          ctg <- suppressWarnings(readALSLAScatalog(paste0(outPath, '.laz')))
          opt_output_files(ctg) <- outPath
          opt_laz_compression(ctg) <- TRUE
          filtered <- filter_duplicates(ctg)
        }
        
      }

    }
    
    # Delete the source data if requested
    if (deleteSource) {
      unlist(list.files(lasPath, full.names = TRUE))
    }
    
  }
  
}

# Function to deteremine the frequent tree types
frequentTrees <- function(fr_boundaries = NULL, fr_cores = NULL, fr_samples = NULL, level = 'reserve') {
  # Argument 'level' determines at what level the freuncy of trees are calculated. 
  # The frequent trees and subsequent forest type will always be calculated at reserve level. 
  # Setting to 'core' or 'samplePlot' will calculate those respectively.
  # Setting to 'all' will calculate for reserve, core and sample plot level.
  
  # Note: will always overwrite!
  
  # Error handling
  if (is.null(fr_boundaries)) {
    stop("Error, must provide forest reserve boundaries.")
  }
  if (!inherits(fr_boundaries, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  if (is.null(fr_cores) && ((level == 'core') | (level == 'all'))) {
    stop("Error, must provide forest reserve core areas")
  }
  if (!inherits(fr_cores, 'sf') && ((level == 'core') | (level == 'all'))) {
    stop("fr_cores must be an sf object.")
  }
  if (is.null(fr_samples)) {
    stop("Error, must provide forest reserve sample plots")
  }
  if (!inherits(fr_samples, 'sf')) {
    stop("fr_samples must be an sf object.")
  }
  if (!level %in% c('reserve', 'core', 'samplePlot', 'all')) {
    stop("Argument 'level' must be one of 'reserve', 'core', 'samplePlot' or 'all")
  }
  
  # Retrieve data
  dataPath <- '../Data/Database/'
  tblSamplePlotsCanopy <- read.csv(paste0(dataPath, 'tblSamplePlotsCanopy.csv'))
  if ((level == 'core') | (level == 'all')) {
    tblCoreAreaCanopy <- read.csv(paste0(dataPath, 'tblCoreAreaCanopy.csv'))
  }
  tblTreeSpecies <- read.csv(paste0(dataPath, 'tblTreeSpecies.csv'))
  
  # Create folder(s) if it doesn't exist yet
  folderPath <- '../Data/FrequentTrees/'
  if (!dir.exists(folderPath)) {
    dir.create(folderPath)
  }
  
  # Subfunction to determine the forest type based on the frequent trees
  determineType <- function(treeType) {
    if ((sum(treeType == 'L') == 0) && (sum(treeType == 'N') == 0)) {
      # No data
      return('NA')
    } else if (sum(treeType == 'L') == length(treeType)) {
      # All three species are deciduous
      return('Deciduous')
    } else if (sum(treeType == 'N') == length(treeType)) {
      # All three species are coniferous
      return('Coniferous')
    } else {
      # Mix between tree types
      return('Mixed')
    }
  }
  
  # Subfunction to determine if one specific tree is high frequency
  determineDominance <- function(topThree, totalCount) {
    
    # Checks if the first of the top three is a frequent species
    # Dominance defined as >50% of the plot trees
    if (totalCount == 0) {
      return('No tree information in plot')
    } else {
      if (round((topThree[1, ]$Freq / totalCount * 100), 2) >= 50) {
        return(tblTreeSpecies$BS_NAAM[(tblTreeSpecies$BS_CODE == topThree[1, ]$Var1)])
      } else {
        return('No dominant species')
      }
    }
  }
  
  # Create a dataframe to save the results to
  rsvFrequentSpecies <- data.frame(RSVCODE = integer(),
                                   frequentSpecies = character(),
                                   forestType = character())
  
  # Loop through the RSVs
  for (i in 1:nrow(fr_boundaries)) {
    # Retrieve RSV code
    rsv <- fr_boundaries[i, ]$RSVCODE
    
    # Check if folder structure exists
    rsvFolderPath <- paste0(folderPath, 'RSV_', rsv, '/')
    if (!dir.exists(rsvFolderPath)) {
      dir.create(rsvFolderPath)
    }
    
    # Create optional dataframes to save core and sample results to
    if ((level == 'core') | (level == "all")) {
      coreFrequentSpecies <- data.frame(RSVCODE = integer(),
                                        frequentSpecies = character(),
                                        topThreeSpecies = character(),
                                        forestType = character())
    }
    
    if ((level == 'samplePlot') | (level == "all")) {
      plotFrequentSpecies <- data.frame(RSVCODE = integer(),
                                        RUIT_COORD = character(),
                                        frequentSpecies = character(),
                                        topThreeSpecies = character(),
                                        forestType = character())
    }
    
    # Get the subset of sample plots for current reserve and subset to get most recent year only
    samples <- tblSamplePlotsCanopy[(tblSamplePlotsCanopy$RSV_CODE == rsv), ]
    samples <- samples[samples$JAAR == max(unique(samples$JAAR)), ]
    
    # Optionally, get the core area subset
    if (exists("coreFrequentSpecies")) {
      core <- tblCoreAreaCanopy[(tblCoreAreaCanopy$RSV_CODE == rsv), ]
      core <- core[core$JAAR == max(unique(core$JAAR)), ]
    }
    
    ### Determine the frequent species and forest type for the whole reserve ###
    
    # Count the amount of each species in all sample sites combined
    counts <- table(samples$BS_CODE)
    
    # Select the tree most present trees as 'dominant' trees
    frequent <- as.data.frame(sort(counts, decreasing = TRUE)[1:3])
    
    # Get the three most frequent tree species names
    species <- tblTreeSpecies$BS_NAAM[(tblTreeSpecies$BS_CODE %in% frequent$Var1)]
    
    # Get the tree type of the three dominant trees
    treeType <- tblTreeSpecies$LoofNaald[(tblTreeSpecies$BS_CODE %in% frequent$Var1)]
    
    # Determine the forest type using subfunction
    forestType <- determineType(treeType)
    
    # Add to the dataframe
    rsvFrequentSpecies <- rbind(rsvFrequentSpecies,
                                data.frame(RSV_CODE = rsv,
                                           frequentSpecies = paste(species, collapse = ', '),
                                           forestType = forestType))
    
    
    ### Optionally, determine the frequent species and forest type for the core area ###
    if (exists("coreFrequentSpecies")) {
      
      # Determine the amount of trees per species
      counts <- table(core$BS_CODE)
      
      # Get the top three species
      topThree <- as.data.frame(sort(counts, decreasing = TRUE)[1:3])
      
      # Determine what percentage of the area the top three species are
      totalCount <- sum(counts)
      
      # Get the species names
      species <- tblTreeSpecies$BS_NAAM[(tblTreeSpecies$BS_CODE %in% topThree$Var1)]
      
      # Get the three types
      treeType <- tblTreeSpecies$LoofNaald[(tblTreeSpecies$BS_CODE %in% topThree$Var1)]
      
      # Determine the forest type
      forestType <- determineType(treeType)
      
      # Determine if there is a most frequent species
      if (length(species) > 1) {
        frequentSpecies <- determineDominance(topThree, totalCount)
      } else {
        frequentSpecies <- species[1]
      }
      
      
      # Add to the dataframe
      coreFrequentSpecies <- rbind(coreFrequentSpecies,
                                   data.frame(RSV_CODE = rsv,
                                              frequentSpecies = frequentSpecies,
                                              topThreeSpecies = paste(species, collapse = ', '),
                                              forestType = forestType))         
      
      
      # Save the file
      write.csv(coreFrequentSpecies, 
                file = paste0(rsvFolderPath, 'CoreFrequentTrees.csv'), 
                row.names = FALSE)
      
    }
    
    ### Optionally, determine the frequent species and forest type for the sample plots ###
    
    if (exists("plotFrequentSpecies")) {
      
      # Get unique RUIT COORDS
      coords <- unique(samples$RUIT_COORD)
      
      # Loop through the sample plots
      for (coord in coords) {
        
        # Subset the data again
        subSamples <- samples[(samples$RUIT_COORD == coord),]
        
        # Calculate frequent trees like before
        counts <- table(subSamples$BS_CODE)
        topThree <- as.data.frame(sort(counts, decreasing = TRUE)[1:3])
        totalCount <- sum(counts)
        species <- tblTreeSpecies$BS_NAAM[(tblTreeSpecies$BS_CODE %in% topThree$Var1)]
        treeType <- tblTreeSpecies$LoofNaald[(tblTreeSpecies$BS_CODE %in% topThree$Var1)]
        forestType <- determineType(treeType)
        if (length(species) > 1) {
          frequentSpecies <- determineDominance(topThree, totalCount)
        } else {
          frequentSpecies <- species[1]
        }
        
        # Add to the dataframe
        plotFrequentSpecies <- rbind(plotFrequentSpecies, data.frame(RSVCODE = rsv,
                                                                      RUIT_COORD = coord,
                                                                      frequentSpecies = frequentSpecies,
                                                                      topThreeSpecies = paste(species, collapse = ', '),
                                                                      forestType = forestType))
        
      }
      
      # Save the file
      write.csv(plotFrequentSpecies, 
                file = paste0(rsvFolderPath, 'PlotFrequentTrees.csv'), 
                row.names = FALSE)
        
    }
    
  }
  
  # Save the reserves result
  write.csv(rsvFrequentSpecies, 
            file = paste0(folderPath, 'FrequentTreesPerRSV.csv'), 
            row.names = FALSE)
  
  
}

# Function to create rasters (DTM, DSM, CHM)
createRasters <- function(fr_boundaries = NULL, resolution = 0.5, overwriteRaster = FALSE) {
  
  # Error handling
  if (is.null(fr_boundaries)) {
    stop("Error, must provide forest reserve boundaries.")
  }
  if (!inherits(fr_boundaries, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  
  # Loop through the forest reserves
  for (i in 1:nrow(fr_boundaries)) {
    
    # Get the RSV code
    rsv <- fr_boundaries[i, ]$RSVCODE
    print(paste0('Processing rasters for RSV_', rsv))
    
    rasterPath <- paste0('../Data/AHN/RSV_', rsv, '/Rasters/')
    
    # Create folder if it doesn't exist yet
    if (!file.exists(rasterPath)) {
      dir.create(rasterPath)
    } else {
      print(paste0('Raster folder already exists - moving on to processing.'))
    }
    
    # Constructing path to clipped files
    clipPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/')
    clipList <- list.files(clipPath, full.names = TRUE)
    
    
    # Check if clipped files exist
    if (length(clipList) == 0) {
      stop(paste0('No clipped files detected, please clip data for RSV_', rsv))
    }
    
    # Create rasters
    for (i in 1:length(clipList)) {
      
      # Read in the las file
      ctg <- suppressWarnings(readALSLAScatalog(clipList[i]))
      
      # Extract the gridcode
      clipName <- str_extract(clipList[i], "(?<=/Clipped/)[^.]+(?=\\.laz)")
      
      # Create file names
      dtmName <- paste0(clipName, '_DTM_', gsub("\\.", "_", as.character(resolution)), 'm')
      dsmName <- paste0(clipName, '_DSM_', gsub("\\.", "_", as.character(resolution)), 'm')
      chmName <- paste0(clipName, '_CHM_', gsub("\\.", "_", as.character(resolution)), 'm')
        
      # Check if the DTM exists, else create and save
      if (!file.exists(paste0(rasterPath, dtmName, '.tif')) | overwriteRaster) {
        
        # Unlink the file if it exists
        unlink(paste0(rasterPath, dtmName, '.tif'))
        
        # Create the DTM
        opt_output_files(ctg) <- paste0(rasterPath, dtmName)
        dtm <- rasterize_terrain(ctg, res = resolution)
        
      } else {
        
        # Load in the existing raster
        dtm <- rast(paste0(rasterPath, dtmName, '.tif'))
        
      }
      
      # Check if the DSM exists, else create and save
      if (!file.exists(paste0(rasterPath, dsmName, '.tif')) | overwriteRaster) {
        
        # Unlink the file if it exists
        unlink(paste0(rasterPath, dsmName, '.tif'))
        
        # Create the DSM
        opt_output_files(ctg) <- paste0(rasterPath, dsmName)
        dsm <- pixel_metrics(ctg, ~max(Z), res = resolution)
        
      } else {
        
        # Load in the existing raster
        dsm <- rast(paste0(rasterPath, dsmName, '.tif'))
        
      }
      
      # Check if the CHM exists, else create and save
      if (!file.exists(paste0(rasterPath, chmName, '.tif')) | overwriteRaster) {
        
        # Check if the extents match
        if (ext(dsm) != ext(dtm)) {
          
          # Clipping the dsm to match
          dsm <- crop(dsm, dtm)
          warning("Extents for ", clipName, " do not match, clipping.")
        
        }
        
        # Create the CHM
        chm <- dsm - dtm
        terra::writeRaster(chm, filename = paste0(rasterPath, chmName, '.tif'), overwrite = overwriteRaster)
          
      }

    }
    
  }
  
}

# Function for ITD
individualTreeDetection <- function(fr_samples, subset = FALSE, subSamples = NULL, overwriteITD = FALSE, saveImg = TRUE) {
  
  # Error handling
  if (is.null(fr_samples)) {
    stop("Error, must provide forest reserve boundaries.")
  }
  if (!inherits(fr_samples, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  if (subset && is.null(subSamples)) {
    stop("Please provide a vector of RUIT_COORD values to subset.")
  }
  
  # Create folder for saving if it doesn't exist yet
  if (!dir.exists('../Data/ITD/')) {
    dir.create('../Data/ITD/')
  }
  
  # Get the unique RSV codes
  rsvs <- unique(fr_samples$rsv_code)
  
  # Loop through the RSVs
  for (rsv in rsvs) {
    
    # Check if a folder exists for this RSV yet
    rsvPath <- paste0('../Data/ITD/RSV_', rsv)
    if (!dir.exists(rsvPath)) {
      dir.create(rsvPath)
    }
    
    # Take subset of the samples
    samples <- fr_samples[fr_samples$rsv_code == rsv,]
    
    # Subset further if requested
    if (subset) {
      
      samples <- samples[samples$RUIT_COORD %in% subSamples, ]
      
    }
    
    # Get the unique RUIT_COORDs
    coords <- unique(samples$RUIT_COORD)

    # Load in the frequent trees information
    freqPath <- paste0('../Data/FrequentTrees/RSV_', rsv, '/PlotFrequentTrees.csv')
    if (file.exists(freqPath)) {
      freqTrees <- read.csv(freqPath)
    } else {
      stop(print(paste0("No frequent tree information found for RSV ", rsv, ", please run frequentTrees() first.")))
    }
    
    # Loop through the RUIT_COORD values
    for (coord in coords) {

      print(coord)
      
      # Check if the file exists for this plot
      filePath <- paste0(rsvPath, '/ITD_', coord, '.csv')
      if (!file.exists(filePath) | overwriteITD) {
        
        # Retrieve the CHM
        chmPath <- paste0('../Data/AHN/RSV_', rsv, '/Rasters/', coord, '_CHM_0_5m.tif')
        if (!file.exists(chmPath)) {
          
          # Warn that this RUIT_COORD will be skipped
          warning('CHM for RSV_' , rsv, ' RUIT_COORD ', coord, ' not found. Skipping!')
          
        } else {
          
          # Read in the CHM
          chm <- rast(chmPath)

          # Geting the forest type
          # Defaults to reserve forest type if information is not available
          forestType <- freqTrees[freqTrees$RUIT_COORD == coord,]$forestType
          if (length(forestType) == 0) {
            reserveTrees <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
            forestType <- reserveTrees[reserveTrees$RSV_CODE == rsv,]$forestType
            message(paste('RSV', rsv, 'coord', coord, 'does not know what forest type it is. Defaulting to reserve forest type.'))
          } else if (is.na(forestType)) {
            reserveTrees <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
            forestType <- reserveTrees[reserveTrees$RSV_CODE == rsv,]$forestType
            message(paste('RSV', rsv, 'coord', coord, 'does not know what forest type it is. Defaulting to reserve forest type.'))
          }

          # Setting parameters based on the forest type
          if (forestType == 'Mixed') {
            a <- -0.856
            b <- 0.178
          } else if (forestType == 'Deciduous') {
            a <- 1.786
            b <- 0.056
          } else if (forestType == 'Coniferous') {
            a <- -0.843
            b <- 0.145
          } else {
            stop(paste("Error, plot", coord, "in RSV", rsv, "does not have a valid forest type."))
          }
          
          # Locate treetops
          func <- function(x) {pmax(2.5, a + (b * x))}
          ttops <- locate_trees(chm, lmf(ws = func, shape="circular", hmin=4))          
          
          # Filter the ttops to only include those within the AOI (remove the 5 m edge effect buffer)
          sample <- samples[samples$RUIT_COORD == coord,]
          aoi <- st_buffer(sample, 12.6)
          aoi <- st_transform(aoi, crs(ttops))
          ttops <- st_filter(ttops, aoi)
          ttops$treeID <- (1:nrow(ttops))
          
          # Get ttops coords for plotting
          ttops_coords <- st_coordinates(ttops)
          
          # write the results
          write.table(ttops, file = filePath, sep = ";", row.names = FALSE, col.names = TRUE, dec = ".")
          
          if (saveImg) {
          
            # File paths
            pngPath2D <- paste0(rsvPath, '/ITD_', coord, '_2d.png')
            pngPath3D <- paste0(rsvPath, '/ITD_', coord, '_3d.png')
            
            # Make a 2D plot
            chmdf <- na.omit(as.data.frame(chm, xy=TRUE))
            colnames(chmdf)[3] <- 'Height'
            p_2d <-ggplot() +
              geom_raster(data=chmdf, aes(x = x, y = y, fill = Height)) +
              scale_fill_viridis_c(name = "Height (m)") +
              geom_sf(data = aoi, color='red', fill=NA, linewidth=1) +
              geom_sf(data = ttops, shape = 3, color = "black", size = 5) +
              annotation_scale(
                location = 'bl', 
                width_hint = 0.3,
                style = 'ticks',
                pad_x = unit(0.2, "cm"),
                pad_y = unit(0.2, "cm")
              ) +
              labs(title = paste0("ITD for ", coord, ' - Canopy view (above)')) +
              theme_minimal() +
              theme(
                legend.position = "right",
                plot.title = element_text(face = "bold", hjust = 0.5)
              )
            
            ggsave(pngPath2D, plot = p_2d, width = 2000, height = 2000, units = "px", dpi = 300)
            
            # Load in the point cloud for plotting
            las <- readALSLAS(paste0('../Data/AHN/RSV_', rsv, '/Clipped/', coord, '.laz'))
            las <- filter_poi(las, Classification != 2)
            
            # Make a 3D plot
            colors <- viridis::viridis(100)[cut(las$Z, breaks = 100, labels = FALSE)]
            open3d()
            clear3d()
            par3d(windowRect = c(20, 20, 1000, 1000))
            plot3d(las$X, las$Y, -las$Z,
                  col = colors,
                  size = 9,
                  xlab = "", ylab = "", zlab = "",
                  axes = FALSE,
                  box = FALSE,
                  aspect = TRUE)
            
            # Plot AOI
            aoi_coords <- st_coordinates(aoi$geometry)
            lines3d(aoi_coords[, 'X'], aoi_coords[, 'Y'], (-min(las$Z)), col='red', lwd = 4)
            
            # Plot identified ttops
            points3d(ttops_coords[, 'X'], ttops_coords[, 'Y'], (-min(las$Z)), col='black', size = 5)
            
            # Add 1.5m circles around the tree tops
            for (i in 1:nrow(ttops)) {
              top_coords <- st_coordinates(st_buffer(ttops[i,], 1))
              lines3d(top_coords[, 'X'], top_coords[, 'Y'], (-min(las$Z) - 0.5), col='black', lwd = 3)
            }
            
            # Add Tree ID labels
            text3d(ttops_coords[, 'X'], 
                  ttops_coords[, 'Y'], 
                  (-min(las$Z) - 1), 
                  texts = ttops$treeID, 
                  col='black',
                  font = 2,
                  cex = 2,
                  pos = 3)
            
            # Add scale bar
            bbox <- par3d("bbox")
            x_range <- bbox[2] - bbox[1]
            y_range <- bbox[4] - bbox[3]
            scale_len <- 10
            scale_label <- paste(scale_len, "m")
            scale_x_start <- bbox[1] + x_range * 0.01
            scale_y_pos <- bbox[3] + y_range * 0.01
            segments3d(x = c(scale_x_start, scale_x_start + scale_len),
                      y = c(scale_y_pos, scale_y_pos),
                      z = -min(las$Z - 0.5),
                      col = "black", lwd = 3)
            text3d(x = scale_x_start + scale_len / 2,
                  y = scale_y_pos,
                  z = -min(las$Z) - 0.05,
                  texts = scale_label,
                  col = "black",
                  adj = c(0.5, 1))
            
            # Add title
            title_x <- bbox[1] + x_range * 0.5
            title_y <- bbox[4] + y_range * 0.05
            title_z <- -min(las$Z)
            text3d(x = title_x, y = title_y, z = title_z,
                  texts = paste0('ITD for ', coord, ' - Stem view (below)'),
                  font = 2,
                  cex = 2)
            
            # Adjust viewing angle 
            view3d(theta = 0, phi = 0, fov = 0, zoom = 0.5)
            
                      
            # Save plot
            snapshot3d(filename = pngPath3D,
                        width = 1000,
                        height = 1000)
            close3d()

          }
          
        }
        
      }
      
    }
    
  }
  
}

# Function to determine the canopy projections
canopyProjection <- function(fr_samples, subset = FALSE, subSamples = NULL, overwriteCP = FALSE, saveImg = TRUE) {

  # Errror handling
  if (is.null(fr_samples)) {
    stop("Error, must provide forest reserve boundaries.")
  }
  if (!inherits(fr_samples, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  if (subset && is.null(subSamples)) {
    stop("Please provide a vector of RUIT_COORD values to subset.")
  }

  # Create folder for saving if it doesn't exist yet
  if (!dir.exists('../Data/CanopyProjection/')) {
    dir.create('../Data/CanopyProjection/')
  }

  # Set the resolution for the CHM
  resolution <- 1

  # Get the unique RSV code
  rsv <- unique(fr_samples$rsv_code)

  # Check if a folder exists for this RSV yet
  rsvPath <- paste0('../Data/CanopyProjection/RSV_', rsv)
  if (!dir.exists(rsvPath)) {
    dir.create(rsvPath)
  }
  
  # Take subset of the samples
  samples <- fr_samples[fr_samples$rsv_code == rsv,]
  
  # Subset further if requested
  if (subset) {
    samples <- samples[samples$RUIT_COORD %in% subSamples, ]
  }
  
  # Get the unique RUIT_COORDs
  coords <- unique(samples$RUIT_COORD)
  
  # Loop through the RUIT_COORD values
  for (coord in coords) {

    # Construct the save file path
    if (!dir.exists(paste0(rsvPath, '/Shapefiles/'))) {
      dir.create(paste0(rsvPath, '/Shapefiles/'))
    }
    saveImgMCWSPath <- paste0(rsvPath, '/', coord, '_mcws.png')
    saveMCWSPath <- paste0(rsvPath, '/Shapefiles/', coord, '_mcws.shp')
    saveImgLi2012Path <- paste0(rsvPath, '/', coord, '_li2012.png')
    saveLi2012Path <- paste0(rsvPath, '/Shapefiles/', coord, '_li2012.shp')

    if (file.exists(saveImgMCWSPath) && file.exists(saveMCWSPath) && !overwriteCP) {
      print(paste0('Canopy projection for RSV_', rsv, ' RUIT_COORD ', coord, ' already exists. Skipping.'))
      next
    }

    # Check if the CHM already exists for this plot
    chmPath <- paste0('../Data/AHN/RSV_', rsv, '/Rasters/', coord, '_pitfree_CHM_0_5m.tif')
    if (file.exists(chmPath) && !overwriteCP) {

      # Read in the CHM
      chm <- rast(chmPath)

    } else {

      # Check if LAS exists
      lasPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/', coord, '.laz')
      if (!file.exists(lasPath)) {
        stop(paste0('LAS file for RSV_', rsv, ' RUIT_COORD ', coord, ' not found. Please clip data first!'))
      }

      # Read in the LAS file
      las <- readALSLAS(lasPath, select = "xyzrc")

      # Normalize height
      las <- normalize_height(las, algorithm = tin())

      # Create gap-filled CHM
      chm <- rasterize_canopy(las, res = 0.5, pitfree(thresholds = c(0, 2, 5, 10, 15)), max_edge = c(0, 1.5))

      # Save the gap- filled CHM
      writeRaster(chm, filename = chmPath, overwrite = TRUE)

    } 

    # Aggregate the CHM to 1m
    chm1m <- aggregate(chm, fact = 2, fun = mean, na.rm = TRUE) 

    # Check if ITD has been performed
    itdPath <- paste0('../Data/ITD/RSV_', rsv, '/ITD_', coord, '.csv')
    if (!file.exists(itdPath)) {
      stop(paste0('ITD for RSV_', rsv, ' RUIT_COORD ', coord, ' not found. Please perform ITD first!'))
    }

    # Load in the CSV and extract the X and Y coordinates
    itd <- read.csv(itdPath, header = TRUE, sep = ';')
    itd <- itd %>% 
      mutate(geometry = gsub("[c()]", "", geometry)) %>%
      separate(geometry, into = c('X', 'Y'), sep = ",\\s*", convert = TRUE)
    itd_sf <- st_as_sf(itd, coords = c('X', 'Y'), crs = st_crs(chm1m))

    # Get the AOI
    sample <- samples[samples$RUIT_COORD == coord,]
    aoi <- st_buffer(sample, 12.6)
    aoi <- st_transform(aoi, crs(chm1m))

    # Mask the CHM to only include the area of interest (not buffered)
    chm1m <- mask(chm1m, aoi)

    # Apply the multicore watershed segmentation from ForestTools
    crowns <- mcws(itd_sf, chm1m, minHeight = (min(itd_sf$Z) - 5))
    crown_df <- as.data.frame(crowns, xy = TRUE)
    colnames(crown_df)[3] <- 'CrownID'

    # Convert the raster into a vector for plotting
    crowns_poly <- as.polygons(crowns, dissolve = TRUE)
    crowns_poly <- st_as_sf(crowns_poly)
    colnames(crowns_poly)[1] <- 'CrownID'

    # Prepare the CHM for plotting
    chm_df <- as.data.frame(chm, xy = TRUE)
    colnames(chm_df)[3] <- 'height'

    # Plot the results of the mcws
    crownPlot <- ggplot() +
      geom_raster(data = chm_df, aes(x = x, y = y, fill = height, alpha = height)) +
      scale_fill_gradient(name = 'Normalised height (m)', low = 'grey20', high = 'grey70') +
      scale_alpha(range = c(0.5, 0.5), guide = "none") +
      new_scale_fill() +
      geom_sf(data = crowns_poly, aes(fill = as.factor(CrownID), color = as.factor(CrownID)), linewidth = 1.5, alpha = 0.3) +
      scale_fill_viridis_d(name = 'Crown ID', option = 'D', na.value = 'grey90') + 
      scale_color_viridis_d(name = 'Crown ID', option = 'D', na.value = 'grey90') +
      geom_sf(data = itd_sf, shape = 3, color = 'black', size = 5) +
      geom_sf(data = aoi, color='red', fill=NA, linewidth=1) +
      annotation_scale(
        location = 'bl',
        width_hint = 0.3,
        style = 'ticks',
        pad_x = unit(0.2, "cm"),
        pad_y = unit(0.2, "cm")
      ) +
      labs(title = paste0('Crown segmentation for ', coord, ' at 1m resolution through MCWS')) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5, vjust = 10)
      )
    
    # Save the plot and the crowns
    if (saveImg) {
      ggsave(filename = saveImgMCWSPath, plot = crownPlot, width = 2500, height = 2500, units = "px", dpi = 300)
    }
    st_write(crowns_poly, saveMCWSPath, append = FALSE)

    # Reload the LAS without ground points
    las <- readALSLAS(lasPath, select = "xyzc", filter = "-drop_class 2")

    # Perform the lidR segmentation for validation
    trees <- segment_trees(las, algorithm = li2012())
    crowns <- pixel_metrics(trees, treeID[which.max(Z)], res = 1, na.rm = TRUE)
    crowns_poly <- crown_metrics(trees, func= .stdtreemetrics, geom="concave")
    crowns_poly <- st_intersection(aoi, crowns_poly)

    # Plot and save
    crownPlot <- ggplot() +
      geom_raster(data = chm_df, aes(x = x, y = y, fill = height, alpha = height)) +
      scale_fill_gradient(name = 'Normalised height (m)', low = 'grey20', high = 'grey70') +
      scale_alpha(range = c(0.5, 0.5), guide = "none") +
      new_scale_fill() +
      geom_sf(data = crowns_poly, aes(fill = as.factor(treeID), color = as.factor(treeID)), linewidth = 1.5, alpha = 0.3) +
      scale_fill_viridis_d(name = 'Crown ID', option = 'D', na.value = 'grey90') + 
      scale_color_viridis_d(name = 'Crown ID', option = 'D', na.value = 'grey90') +
      geom_sf(data = itd_sf, shape = 3, color = 'black', size = 5) +
      geom_sf(data = aoi, color='red', fill=NA, linewidth=1) +
      annotation_scale(
        location = 'bl',
        width_hint = 0.3,
        style = 'ticks',
        pad_x = unit(0.2, "cm"),
        pad_y = unit(0.2, "cm")
      ) +
      labs(title = paste0('Crown segmentation for ', coord, ' at point cloud level using Li 2012 algorithm')) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5, vjust = 10)
      )

    if (saveImg) {
      ggsave(filename = saveImgLi2012Path, plot = crownPlot, width = 2500, height = 2500, units = "px", dpi = 300)
    }
    st_write(crowns_poly, saveLi2012Path, append = FALSE)

  }

}

# Function to detect snag trees
snagDetection <- function(fr_samples, subset = FALSE, subSamples = NULL, heightThreshold = 4, plotting = FALSE) {

  # Error handling
  if (is.null(fr_samples)) {
    stop("Error, must provide forest reserve boundaries.")
  } 
  if (!inherits(fr_samples, 'sf')) {
    stop("fr_boundaries must be an sf object.")
  }
  if (subset && is.null(subSamples)) {
    stop("Please provide a vector of RUIT_COORD values to subset.")
  }

  # Create folder for saving if it doesn't exist yet
  rsvs <- unique(fr_samples$rsv_code)
  if (!dir.exists('../Data/Snags/')) {
    dir.create('../Data/Snags/')
  }
 
  # Loop through the RSVs
  for (rsv in rsvs) {
  
    # Folder creation
    if (!dir.exists(paste0('../Data/Snags/RSV_', rsv))) {
      dir.create(paste0('../Data/Snags/RSV_', rsv))
    } 

    # Subsetting if requested
    if (subset) {
      samples <- fr_samples[fr_samples$rsv_code == rsv & fr_samples$RUIT_COORD %in% subSamples, ]
    } else {
      samples <- fr_samples[fr_samples$rsv_code == rsv, ]
    }

    # Loop through the sample sites
    for (coord in unique(samples$RUIT_COORD)) {
    
      # Check if the file exists for this plot
      saveSnagPath <- paste0('../Data/Snags/RSV_', rsv, '/Snag_', coord, '.csv')
      if (file.exists(saveSnagPath) && !overwriteSnag) {
        print(paste0('Snag detection for RSV_', rsv, ' RUIT_COORD ', coord, ' already exists. Skipping.'))
        next
      }

      # Load in the point cloud
      lasPath <- paste0('../Data/AHN/RSV_', rsv, '/Clipped/', coord, '.laz')
      las <- readALSLAS(lasPath, select = "xyzric")

      # Normalise the height
      las <- normalize_height(las, algorithm = tin())

      # Drop all points that are not 1st or 2nd returns and drop the ground class
      las <- filter_poi(las, ReturnNumber %in% c(1, 2))

      # Convert the intensity values to the intensity index
      IntensityIndex <- round((las$Intensity - min(las$Intensity, na.rm = TRUE)) / (max(las$Intensity, na.rm = TRUE) - min(las$Intensity, na.rm = TRUE)) * 255)
      las <- add_lasattribute(las, IntensityIndex, "IntensityIndex", "Normalised intensity")

      ### Stage one: Determine / calcuate the plot-level lidar variables ###
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
      rm(las)

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
      plot(snags, color="snagCls", colorPalette = rainbow(5)[-1])

    }

  }

}

# Function to preprocess the fieldwork data
preprocessFielddata <- function() {

  # Load in the raw field work data
  dataNorgerholt <- read_excel('../Data/FieldworkData/Invoerformulieren Norgerholt 2021 (Andre & Otto).xlsx')
  dataPijpebrandje <- read_excel('../Data/FieldworkData/Invoerformulieren PIJ2018 & HLE20182019 + TON2019.xlsx', sheet = "Plotpnamen Pijpebrandje")
  dataLeesten <- read_excel('../Data/FieldworkData/Invoerformulieren PIJ2018 & HLE20182019 + TON2019.xlsx', sheet = "Plotopnamen Leesten")

  ### Organise the data, leaving only the relevant columns and rows ###
  
  ## Norgerholt ##
  dataNorgerholtNames <- dataNorgerholt[1,]
  names(dataNorgerholt) <- dataNorgerholtNames
  names(dataNorgerholt)[21:34] <- paste0(names(dataNorgerholt)[21:34], "_2021")   # Add suffix to the recent column names
  dataNorgerholt <- dataNorgerholt[-1,] # Drop the first row that contains no data
  dataNorgerholt <- dataNorgerholt[dataNorgerholt$Vitaliteit_2021 %in% c(0, 1, 2, 3) | dataNorgerholt$Schade_2021 %in% c(7, 8),]# Select only standing trees (living or dead)
  dropCols <- c('Reservaatcode', 'BS Code', 'Tophoek', 'Topafstand', 'Vorm', 'Scheuttal Groot', 'Kroon Gemeten', 'Aanzethoogte [m]', 'Stamlengte [m]', 'Tophoek [graden]', 'Topafstand [dm]', 'Vertering', 'Opnamedatum_2021', 'Tophoek [graden]_2021', 'Topafstand [dm]_2021', 'Vorm_2021', 'Scheuttal Groot_2021', 'Kroon Gemeten_2021', 'Aanzethoogte [m]_2021', 'Stamlengte [m]_2021', 'Vertering_2021') # Specify the columns to be dropped
  dataNorgerholt <- dataNorgerholt[, !(names(dataNorgerholt) %in% dropCols)] # Drop the columns
  dataNorgerholt <- dataNorgerholt[!grepl("weg", dataNorgerholt$Bijzonderheden_2021, ignore.case = TRUE), ] # Get rid of rows that state the tree is gone

  # Create save directory
  if (!dir.exists('../Data/FieldworkData/Cleaned/RSV_46/')) {
    dir.create('../Data/FieldworkData/Cleaned/RSV_46/')
  }

  coords <- unique(dataNorgerholt$'Ruit Coord') # Get unique coordinates
  for (coord in coords) {
    # Construct file path
    savePath <- paste0('../Data/FieldworkData/Cleaned/RSV_46/', coord, '.csv')

    # Get relevant data
    coordData <- dataNorgerholt[dataNorgerholt$'Ruit Coord' == coord,]
    coordData$recentHeight <- ifelse(
      is.na(coordData$'Tophoogte [m]_2021'),
      coordData$'Tophoogte [m]',
      coordData$'Tophoogte [m]_2021'
    )

    # Write the CSV
    write.csv(coordData, file = savePath, row.names = FALSE)

  }

  ## Pijpebrandje ##
  dataPijpebrandjeNames <- dataPijpebrandje[1,]
  names(dataPijpebrandje) <- dataPijpebrandjeNames
  dataPijpebrandje <- dataPijpebrandje[-1,] # Drop the first row that contains no data
  names(dataPijpebrandje)[21:35] <- paste0(names(dataPijpebrandje)[21:35], "_2018")   # Add suffix to the recent column names
  dataPijpebrandje <- dataPijpebrandje[, -(c(1, (ncol(dataPijpebrandje)-6):ncol(dataPijpebrandje)))] # Drop empty columns at end and start
  dataPijpebrandje <- dataPijpebrandje[dataPijpebrandje$Vitaliteit_2018 %in% c(0, 1, 2, 3) | dataPijpebrandje$Schade_2018 %in% c(7, 8),]# Select only standing trees (living or dead)
  dropCols <- c('BS Code', 'Tophoek [graden]', 'Topafstand [dm]', 'Vorm', 'Scheuttal Groot', 'Kroon Gemeten', 'Aanzethoogte [m]', 'Stamlengte [m]', 'Vertering', 'Veldwerker(s)_2018', 'Opnamedatum_2018', 'Tophoek [graden]_2018', 'Topafstand [dm]_2018', 'Vorm_2018', 'Scheuttal Groot_2018', 'Kroon Gemeten_2018', 'Aanzethoogte [m]_2018', 'Stamlengte [m]_2018', 'Vertering_2018') # Specify the columns to be dropped
  dataPijpebrandje <- dataPijpebrandje[, !(names(dataPijpebrandje) %in% dropCols)] # Drop the columns

  # Create save directory
  if (!dir.exists('../Data/FieldworkData/Cleaned/RSV_9/')) {
    dir.create('../Data/FieldworkData/Cleaned/RSV_9/')
  }

  coords <- unique(dataPijpebrandje$'Ruit Coord') # Get unique coordinates
  for (coord in coords) {
    # Construct file path
    savePath <- paste0('../Data/FieldworkData/Cleaned/RSV_9/', coord, '.csv')

    # Get relevant data
    coordData <- dataPijpebrandje[dataPijpebrandje$'Ruit Coord' == coord,]
    coordData$recentHeight <- ifelse(
      is.na(coordData$'Tophoogte [m]_2018') | coordData$'Tophoogte [m]_2018' == '0',
      coordData$'Tophoogte [m]',
      coordData$'Tophoogte [m]_2018'
    )

    # Write the CSV
    write.csv(coordData, file = savePath, row.names = FALSE)

  }

  ## Leesten ##
  dataLeestenNames <- dataLeesten[1,]
  names(dataLeesten) <- dataLeestenNames
  dataLeesten <- dataLeesten[-1,] # Drop the first row that contains no data
  names(dataLeesten)[21:35] <- paste0(names(dataLeesten)[21:35], "_2018")   # Add suffix to the recent column names
  dataLeesten <- dataLeesten[, -(c(1, (ncol(dataLeesten)-1):ncol(dataLeesten)))] # Drop empty columns at end and start
  dataLeesten <- dataLeesten[dataLeesten$Vitaliteit_2018 %in% c(0, 1, 2, 3) | dataLeesten$Schade_2018 %in% c(7, 8),]# Select only standing trees (living or dead)
  dropCols <- c('BS Code', 'Tophoek [graden]', 'Topafstand [dm]', 'Vorm', 'Scheuttal Groot', 'Kroon Gemeten', 'Aanzethoogte [m]', 'Stamlengte [m]', 'Vertering', 'Veldwerker(s)_2018', 'Opnamedatum_2018', 'Tophoek [graden]_2018', 'Topafstand [dm]_2018', 'Vorm_2018', 'Scheuttal Groot_2018', 'Kroon Gemeten_2018', 'Aanzethoogte [m]_2018', 'Stamlengte [m]_2018', 'Vertering_2018') # Specify the columns to be dropped
  dataLeesten <- dataLeesten[, !(names(dataLeesten) %in% dropCols)] # Drop the columns

  # Create save directory
  if (!dir.exists('../Data/FieldworkData/Cleaned/RSV_14/')) {
    dir.create('../Data/FieldworkData/Cleaned/RSV_14/')
  }

  coords <- unique(dataLeesten$'Ruit Coord') # Get unique coordinates
  for (coord in coords) {
    # Construct file path
    savePath <- paste0('../Data/FieldworkData/Cleaned/RSV_14/', coord, '.csv')

    # Get relevant data
    coordData <- dataLeesten[dataLeesten$'Ruit Coord' == coord,]
    coordData$recentHeight <- ifelse(
      is.na(coordData$'Tophoogte [m]_2018') | coordData$'Tophoogte [m]_2018' == '0',
      coordData$'Tophoogte [m]',
      coordData$'Tophoogte [m]_2018'
    )

    # Write the CSV
    write.csv(coordData, file = savePath, row.names = FALSE)

  }


}

# Function to add location data to the fieldwork points to allow comparison against itd
addLocationData <- function() {

  # Get all the cleaned RSVs
  rsvs <- list.dirs('../Data/FieldworkData/Cleaned/', recursive = FALSE)
  rsvCodes <- sub("RSV_", "", basename(rsvs))

  # Get the sample plots and set CRS
  samplePlots <- st_read('../Data/GIS/ReserveSamplePlots.shp', crs=28992)

  # Loop through them
  for (rsv in rsvCodes) {

    # Get the path to the folder which contains the cleaned field work data
    rsvDir <- paste0('../Data/FieldworkData/Cleaned/RSV_', rsv)

    # Extract the sample plots which have cleaned data
    cleanedPlots <- list.files(rsvDir)

    # Loop through the plots
    for (plot in cleanedPlots) {
      
      # Get only the coord number from the filename
      coord <- sub('.csv', "", basename(plot))
      dataPath <- paste0('../Data/FieldworkData/Cleaned/RSV_', rsv, '/', plot)
      
      # Load the data
      plotData <- read.csv(dataPath)
      samplePlotSf <- samplePlots[samplePlots$rsv_code == rsv & samplePlots$RUIT_COORD == coord, ]

      # Get middlepoint of the plot
      middleX <- samplePlotSf$rdx
      middleY <- samplePlotSf$rdy

      # Loop through the rows in the data
      for (i in 1:nrow(plotData)) {

        dist_m <- plotData$Afstand..dm.[i] / 10
        angle_rad <- (90 - plotData$Hoek..graden.[i]) * pi / 100
        
        plotData$rdx[i] <- middleX + (dist_m * cos(angle_rad))
        plotData$rdy[i] <- middleY + (dist_m * sin(angle_rad))

      }

      # Write the results back
      write.csv(plotData, dataPath)
      
    }
    
  }

}

# Analysis of accuracies
accuracyAnalysis <- function(overwriteAccuracy = TRUE) {

  # Get list of cleaned data reserves
  cleanedList <- list.dirs('../Data/FieldworkData/Cleaned', recursive = FALSE)
  rsvCodes <- sub("RSV_", "", basename(cleanedList))

  # Construct file path for saving the accuracy metrics and delete the 'old' version if overwrite is on
  accuracyDir <- '../Data/Accuracy'
  if (!dir.exists(accuracyDir)) {
    dir.create(accuracyDir)
  }
  accuracyPlotPath <- paste0(accuracyDir, '/AccuracyMetricsPlot.csv')
  accuracyTreePath <- paste0(accuracyDir, '/AccuracyMetricsTree.csv')
  if (overwriteAccuracy) {
    if (file.exists(accuracyPlotPath)) {
      unlink(accuracyPlotPath)
    }
    if (file.exists(accuracyTreePath)) {
      unlink(accuracyTreePath)
    }
  }

  # Create empty dataframes for results
  resultsPlot <- data.frame(
    RSV                     = numeric(),
    Plot                    = character(),
    forestType              = character(),
    n_trees_field           = numeric(),
    n_trees_itd             = numeric(),
    avg_height_field        = numeric(),
    avg_height_itd          = numeric(),
    avg_distance_to_nearest = numeric(),
    avg_height_difference   = numeric(),
    stringsAsFactors = FALSE
  )

  resultsTree <- data.frame(
    RSV                      = numeric(),
    Plot                     = character(),
    forestType               = character(),
    tree_id_field            = integer(),
    dist_to_nearest_itd      = numeric(),
    height_diff_with_nearest = numeric(),
    stringsAsFactors = FALSE
)

  for (rsv in rsvCodes) {

    # Load in the frequent trees information
    freqPath <- paste0('../Data/FrequentTrees/RSV_', rsv, '/PlotFrequentTrees.csv')
    if (file.exists(freqPath)) {
      freqTrees <- read.csv(freqPath)
    } else {
      stop(print(paste0("No frequent tree information found for RSV ", rsv, ", please run frequentTrees() first.")))
    }

    rsvPath <- paste0('../Data/FieldworkData/Cleaned/RSV_', rsv)
    coords <- sub('.csv', "", basename(list.files(rsvPath)))

    for (coord in coords) {

      # Geting the forest type
      # Defaults to reserve forest type if information is not available
      forestType <- freqTrees[freqTrees$RUIT_COORD == coord,]$forestType
      if (length(forestType) == 0) {
        reserveTrees <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
        forestType <- reserveTrees[reserveTrees$RSV_CODE == rsv,]$forestType
        message(paste('RSV', rsv, 'coord', coord, 'does not know what forest type it is. Defaulting to reserve forest type.'))
      } else if (is.na(forestType)) {
        reserveTrees <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
        forestType <- reserveTrees[reserveTrees$RSV_CODE == rsv,]$forestType
        message(paste('RSV', rsv, 'coord', coord, 'does not know what forest type it is. Defaulting to reserve forest type.'))
      }

      # Load in the fieldwork data
      fieldData <- read.csv(paste0(rsvPath, '/', coord, '.csv'))

      # Get only the main stem (Volg Nr. 0 = primary stem)
      fieldData <- fieldData[fieldData$Volg.Nr == 0, ]

      # Get the ITD data
      itdPath <- paste0('../Data/ITD/RSV_', rsv, '/ITD_', coord, '.csv')
      itdData <- read.csv(itdPath, sep = ";")

      # Convert into SF objects
      coordinates <- lapply(
        itdData$geometry, 
        function(x) {
          as.numeric(
            unlist(
              regmatches(x, gregexpr("(\\d+\\.?\\d*)", x))
            )
          )
        }
      )
      itdData$rdX <- 0
      itdData$rdY <- 0
      for (i in 1:length(coordinates)) {
        itdData[i,]$rdX <- unlist(coordinates[i])[1]
        itdData[i,]$rdY <- unlist(coordinates[i])[2]
      }
      itdData <- st_as_sf(itdData, coords=c("rdX", "rdY"), crs=28992)
      fieldData <- fieldData[!is.na(fieldData$rdx) & !is.na(fieldData$rdy),]
      fieldData <- st_as_sf(fieldData, coords=c("rdx", "rdy"), crs=28992)

      # Identify the closest tree for each tree and get information from it
      nearest_idx <- st_nearest_feature(fieldData, itdData)
      closest_itd <- itdData[nearest_idx,]
      distances <- st_distance(fieldData, closest_itd, by_element = TRUE)
      nearest_results <- cbind(fieldData, closest_itd %>% st_drop_geometry(), nearest_dist_m = as.numeric(distances))

      # Save the tree information
      for (i in seq_len(nrow(nearest_results))) {
        # Calculate the height difference
        heightField <- nearest_results[i,]$recentHeight
        heightITD <- nearest_results[i,]$Z
        if (is.na(heightField)) {
          heightDiff <- NA
        } else {
          heightDiff <- heightITD - heightField
        }

        # Write a new row with the results
        resultsTree[nrow(resultsTree) + 1,] <- list(rsv, coord, forestType, nearest_results[i,]$BNR, nearest_results[i,]$nearest_dist_m, heightDiff)
      }

      # Plot level calculations
      treeCountField <- nrow(fieldData)
      treeCountITD <- nrow(itdData)
      avgHeightField <- mean(fieldData$recentHeight, na.rm = TRUE)
      avgHeightITD <- mean(itdData$Z, na.rm = TRUE)
      avgDistNearest <- mean(resultsTree[resultsTree$RSV == rsv, ]$dist_to_nearest_itd, na.rm = TRUE)
      avgHeightDiff <- mean(resultsTree[resultsTree$RSV == rsv, ]$height_diff_with_nearest, na.rm = TRUE)

      # Write a new row with the results
      resultsPlot[nrow(resultsPlot) + 1,] <- list(rsv, coord, forestType, treeCountField, treeCountITD, avgHeightField, avgHeightITD, avgDistNearest, avgHeightDiff)

    }

  }

  # Write the results
  write.csv(resultsTree, file=accuracyTreePath)
  write.csv(resultsPlot, file=accuracyPlotPath)

}
