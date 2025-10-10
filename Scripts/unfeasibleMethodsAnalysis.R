##############################
##############################

### Unfeasible methods analysis
### Nik Verweel
### 03/09/2025

##############################
##############################

# Source functions
source('funcs.R')

# Preprocessing - subsetting for SRO2
preprocessingResults <- preproccesing(subset = TRUE, sublist = c(3, 16, 19))

# Unpack results from preprocessing 
fr_samples <- preprocessingResults[[3]]
rm(preprocessingResults)

# Define the coordinates of interest
coordsRSV3 <- c("A04", "G05", "S09")
coordsRSV16 <- c("C05", "E15", "H03")
coordsRSV19 <- c("D02", "G01", "J05")

# Subset the data further and clean up
fr_samples <- fr_samples[(fr_samples$rsv_code == 3 & fr_samples$RUIT_COORD %in% coordsRSV3) | 
                         (fr_samples$rsv_code == 16 & fr_samples$RUIT_COORD %in% coordsRSV16) | 
                         (fr_samples$rsv_code == 19 & fr_samples$RUIT_COORD %in% coordsRSV19),]
rm(coordsRSV3, coordsRSV16, coordsRSV19)

### Crown Projection ###

# Loop through the unique RSVs
for (rsv in unique(fr_samples$rsv_code)) {

    # Construct path to correct LAS folder
    rsvPath <- paste0("../Data/AHN/RSV_", rsv, "/")

    # Loop through the coordinates
    for (coord in unique(fr_samples[fr_samples$rsv_code == rsv,]$RUIT_COORD)) {

        # Construct las path
        lasPath <- paste0(rsvPath, "Clipped/", coord, ".laz")

        # Load in the las without the ground points
        las <- readLAS(lasPath, select = "xysc", filter = "-drop_class 2")
        las_df <- as.data.frame(las@data)
        las_df$colors <- gray.colors(100, start = 1, end = 0)[cut(las_df$Z, breaks = 100, labels = FALSE)]

        # Segment the point cloud with the default Li 2012 algorithm
        trees <- segment_trees(las, algorithm = li2012())
        crowns <- crown_metrics(trees, func = .stdtreemetrics, geom = "concave")
        colours <- viridis(nrow(crowns))

        # Segment the point cloud with the alternate algorithm
        treesAlt <- segment_trees(las, algorithm = li2012(dt1 = 1.5, dt2 = 1.75, Zu = 15, hmin = 2)) # Change these parameters
        crownsAlt <- crown_metrics(treesAlt, func = .stdtreemetrics, geom = "concave")
        coloursAlt <- viridis(nrow(crownsAlt))

        # Setting up plotting window
        open3d(windowRect = c(50, 50, 1200, 600))
        bg3d("white")
        layout3d(matrix(1:2, nrow = 1), sharedMouse = TRUE)

        # Left plot
        next3d()
        points3d(las_df$X, las_df$Y, las_df$Z, color = las_df$colors, size = 4)
        aspect3d("iso")
        text3d(min(trees@data$X) + 17, max(trees@data$Y) + 3, max(trees@data$Z), text = "Default Li 2012 algorithm", adj = 0.5, cex = 0.9, color = "black")
        view3d(userMatrix = rotationMatrix(0, 1, 0, 0), zoom = 0.8)
        material3d(shininess = 0, specular = "black")
        for (id in unique(crowns$treeID)) {
            coords <- st_coordinates(crowns[crowns$treeID == id, ])
            polygon3d(coords[,1], coords[,2], crowns[crowns$treeID == id,]$Z,
                      color = colours[id], alpha = 0.5)
        }

        # Right plot
        next3d()
        points3d(las_df$X, las_df$Y, las_df$Z, color = las_df$colors, size = 4)
        aspect3d("iso")
        text3d(min(trees@data$X) + 17, max(trees@data$Y) + 3, max(trees@data$Z), text = "Adapted Li 2012 algorithm", adj = 0.5, cex = 0.9, color = "black")
        view3d(userMatrix = rotationMatrix(0, 1, 0, 0), zoom = 0.8)
        material3d(shininess = 0, specular = "black")
        for (id in unique(crownsAlt$treeID)) {
            coords <- st_coordinates(crownsAlt[crownsAlt$treeID == id, ])
            polygon3d(coords[,1], coords[,2], crownsAlt[crownsAlt$treeID == id,]$Z,
                      color = coloursAlt[id], alpha = 0.5)
        }

        # Save the plot
        savePath <- paste0("../Figures/Results/SRO3/CrownProjection/", rsv, "_", coord, ".png")
        rgl.snapshot(savePath, fmt = "png")
        close3d()

    }

}

### Snag Detection ###

