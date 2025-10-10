##############################
##############################

### Data Visualisation
### Nik Verweel
### 10/03/2025

##############################
##############################

##### Preprocessing #####

# Import functions
source("ahnProcessingFunctions.R")

# Check required packages
CheckPackages(c('sf', 'dplyr', 'ggplot2', 'ggrepel', 'ggspatial', 'leaflet', 'leaflet.extras', 'xfun', 'htmlwidgets'))

# Load in the Forest Reserve boundaries
fr_boundaries <- st_read("../Data/GIS/ReserveBoundary.shp")
fr_boundaries <- fr_boundaries[fr_boundaries$RSVCODE != 55,] # Removal of excluded Forest Reserve (not yet processed in Database)
fr_boundaries <- st_set_crs(fr_boundaries, 28992)

# Load in the sample plots & subset to include only for FR3
fr_samples <- st_read("../Data/GIS/ReserveSamplePlots.shp")
fr_samples <- st_set_crs(fr_samples, 28992)
fr_samples <- fr_samples[fr_samples$rsv_code == 3,]

# Load in the core areas
fr_core <- st_read("../Data/GIS/ReserveCoreaAreaPolygon.shp")
fr_core <- st_set_crs(fr_core, 28992)
fr_core <- fr_core[fr_core$rsvcode == 3,]

# Dissolving the geometries
fr_boundaries <- fr_boundaries %>%
  group_by(RSVCODE, NAAM) %>%
  summarise(geometry = st_union(geometry))

# Calculating the area again
fr_boundaries$Shape_area <- as.numeric(st_area(fr_boundaries))

# Adding area in hectares information
fr_boundaries$Shape_area_ha <- round((fr_boundaries$Shape_area / 10000), digits = 2)

# Adding AHN Point Cloud Viewer information
fr_boundaries$Link <- NA
for (i in 1:nrow(fr_boundaries)) {
  bbox <- st_bbox(fr_boundaries$geometry[[i]])
  middle_x <- (bbox[1] + bbox[3]) / 2
  middle_y <- (bbox[2] + bbox[4]) / 2
  link <- paste0("https://ns_hwh.fundaments.nl/hwh-ahn/AHN_POTREE/index.html?position=[", 
                 middle_x, 
                 ";", 
                 middle_y, 
                 ";500.00;]&target=[", 
                 middle_x,
                 ";",
                 middle_y,
                 ";-500.00;]")
  fr_boundaries$Link[[i]] <- link
}

# Reprojecting for webmap
fr_boundaries_wsg <- st_transform(fr_boundaries, 4326)

# Get province data
data_URL <- "https://github.com/GeoScripting-WUR/Scripting4GeoIntro/releases/download/data/data.zip"
if (!dir.exists("../Data/Netherlands")){
  dir.create("../Data/Netherlands")}
if (!file.exists('../Data//Netherlands/province.zip')) {
  download.file(url = data_URL, destfile = '../Data/Netherlands/province.zip', method = 'auto', mode = 'wb')
  unzip('../Data/Netherlands/province.zip', exdir = '../Data/Netherlands')
}
province <- read_sf("../Data/Netherlands/gadm41_NLD_1.json")
province$TYPE_1[14] <- 'Provincie' # Fix an error in the dataset
province <- province[province$TYPE_1 == 'Provincie', ]

# Adding centroid data
fr_boundaries_centroid <- st_centroid(fr_boundaries)
province <- st_transform(province, crs = st_crs(fr_boundaries_centroid))
fr_boundaries_centroid_df <- data.frame(st_coordinates(fr_boundaries_centroid))
fr_boundaries_wsg_centroid <- cbind(fr_boundaries_wsg, st_coordinates(st_centroid(fr_boundaries_wsg)))

# Adding forest type information
forestTypes <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
fr_boundaries_centroid <- distinct(left_join(fr_boundaries_centroid, select(forestTypes, RSV_CODE, forestType), by = c("RSVCODE" = "RSV_CODE")))

##### Plotting Static Visualisation #####

# Adding colours
forest_cols <-c (
  "Coniferous" = "#104911",
  "Deciduous" = "#F9A620",
  "Mixed" = "#A8D5E2",
  "NA" = "black"
)

# Plotting
pl <- ggplot() +
  geom_sf(data = province) + 
  geom_sf(
    data = fr_boundaries_centroid,
    aes(color = forestType),
    show.legend = "point"
  ) +  
  scale_color_manual(
    name = "Forest Type",
    values = forest_cols
  ) +
  theme_minimal() + 
  geom_text_repel(
    data = fr_boundaries_centroid,
    aes(
      x = fr_boundaries_centroid_df$X,
      y = fr_boundaries_centroid_df$Y,
      label = RSVCODE
    ), 
    size = 3,
    box.padding = 0.1,
    point.padding = 0.1
  ) + 
  annotation_north_arrow(
    location = "tl", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering()
  ) + 
  annotation_scale(
    location = "bl",         
    width_hint = 0.5,        
    style = "bar",            
    height = unit(0.2, "cm") 
  ) + 
  coord_sf(crs = st_crs(28992), datum = st_crs(28992)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Save the image
ggsave("../Figures/study_area.png")

##### Making webmap #####

# Reading dominant species
frequentSpecies <- read.csv('../Data/FrequentTrees/FrequentTreesPerRSV.csv')
fr_boundaries_wsg <- merge(fr_boundaries_wsg, frequentSpecies, by.x = 'RSVCODE', by.y = "RSV_CODE", all = TRUE)
fr_boundaries_wsg$forestType[is.na(fr_boundaries_wsg$forestType)] <- "Unkown"
fr_boundaries_wsg$frequentSpecies[fr_boundaries_wsg$frequentSpecies == ""] <- "Unkown"

# Leaflet map
map <- leaflet() %>% 
          addTiles() %>% 
          addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
          addTiles(
            urlTemplate = "https://service.pdok.nl/hwh/luchtfotorgb/wmts/v1_0/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.png",
            attribution = "&copy; <a href='https://www.kadaster.nl'>Kadaster</a> | <a href='https://www.pdok.nl/'>PDOK</a>",
            group = "Dutch Aerial (PDOK)",
            options = tileOptions(maxZoom = 19)
          ) %>%
          addPolygons(data = fr_boundaries_wsg, 
                      popup = paste0("Reserve code: ", 
                                     fr_boundaries_wsg$RSVCODE, 
                                     "<br>Reserve name: ",
                                     fr_boundaries_wsg$NAAM, 
                                     "<br>Area: ", 
                                     fr_boundaries_wsg$Shape_area_ha, " ha",
                                     "<br>Forest type: ", fr_boundaries_wsg$forestType,
                                     "<br>Most frequent tree species (by stem count): <i>", fr_boundaries_wsg$frequentSpecies, "</i>",
                                     "<br><a href='", fr_boundaries_wsg$Link, "' target='_blank'>AHN Point Cloud Viewer</a>"),
                      color = "red") %>%
          addMarkers(
            data = fr_boundaries_wsg_centroid, lng = ~X, lat = ~Y, label = fr_boundaries_wsg_centroid$NAAM,
            group = 'reserves',
            icon = makeIcon("http://leafletjs.com/examples/custom-icons/leaf-green.png",
                            iconWidth = 1,
                            iconHeight = 1)
          ) %>%
          addControl(position = "topright",
                     html = "<b>Dutch Forest Reserve Network</b><br>
                             Map by <a href='https://www.nikverweel.nl' target='_blank'>Nik Verweel</a><br>
                             Data Source: Bijlsma, R. J. (2019).<br> <em>Dutch forest reserves database and network</em> [Dataset]<br> https://doi.org/10.17026/dans-2bd-kskz") %>%
          addLegend(position = "topright", colors = "red", labels = "Forest Reserves") %>%
          addLayersControl(
            baseGroups = c("OpenStreetMap", "Dutch Aerial (PDOK)"),
            options = layersControlOptions(collapsed = TRUE),
            position = "bottomright"
          ) %>%
          addSearchFeatures(targetGroups = "reserves", 
                            options = searchFeaturesOptions(zoom = 15, 
                                                            openPopup = TRUE, 
                                                            firstTipSubmit = TRUE, 
                                                            autoCollapse = TRUE,
                                                            hideMarkerOnCollapse = TRUE
          ))

saveWidget(map, file = '../Webmap/index.html', title = "Dutch Forest Reserve Webmap", selfcontained = FALSE)

##### Figure plot setup #####

# Plotting
pl <- ggplot() +
  geom_sf(data = fr_boundaries[fr_boundaries$RSVCODE == 3,],
          fill = 'gray95',
          color = 'black',
          linewidth = 0.8) + 
  geom_sf(data = fr_samples,
          shape = 3,
          color = 'black',
          size = 2) +
  geom_sf(data = fr_core,
          fill = NA,
          color='darkred',
          linewidth = 0.9) +
  theme_minimal() + 
  annotation_north_arrow(
    location = "tl", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering()
  ) + 
  annotation_scale(
    location = "bl",         
    width_hint = 0.5,        
    style = "bar",            
    height = unit(0.2, "cm") 
  ) + 
  coord_sf(crs = st_crs(28992), datum = st_crs(28992)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Save the image
ggsave("../Figures/monitoring_levels.png")
