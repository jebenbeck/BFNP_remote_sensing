## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Rieser
#' Last updated: 2023
#' Status: Work in progress 


### Purpose of script ----


### Notes ----


### Required datasets ----


### Required packages ----

require(tidyverse)
require(sf)
require(terra)
require(dplyr)
require(raster)
require(mapview)
require(units)
library(pbapply)



## 1. Calculate single tree metrics ------------------------------------------------------------------------------------


#' function with processing chain for each file:
calculate_metrics <- function(layername, out_dir, gpkg_name){
  
  #' read in the polygons:
  ST_polygons <- st_read(gpkg_path, layer = layername, quiet = TRUE) #%>% 
      # mutate(VALID = st_is_valid(.)) %>%              #' check individual polygons, if valid
      # filter(VALID == T)                              #' remove invalid polygons
 
  #' data praparation and processing:
  ST_polygons_edit <- ST_polygons %>% 
    rename(
      TREE_ID = ID, 
      TREE_HEIGHT = HEIGHT,
      Z_POS = TER_HEIGHT,
      CROWN_BASE_HEIGHT = CB_HEIGHT,
      CROWN_VOLUME = CROWN_VOL) %>% 
    relocate(Z_POS, .after = Y_POS) %>% 
    relocate(c(TREE_HEIGHT, CROWN_BASE_HEIGHT), .after = TREE_CLASS) %>% 
    mutate(
      #' recode the tree classes:
      TREE_CLASS = recode(TREE_CLASS, 
                          'conif' = 'Coniferous', 
                          'decid' = 'Deciduous',
                          'deadw' = 'Dead',
                          'snag' = 'Snag'),
      
      #' calculate the crown area:
      CROWN_AREA = geom %>% st_area() %>% drop_units(),
      
      #' calculate the crown radius:
      CROWN_RADIUS = 2*sqrt(CROWN_AREA/pi),
      
      #' calculate the DBH:
      DBH = case_when(
        TREE_CLASS == "Coniferous" & Z_POS > 1100  ~ 8.2149 + -0.347989 * TREE_HEIGHT + 0.85412 * CROWN_RADIUS + 0.063952 * TREE_HEIGHT ^ 2 + 0.094879 * CROWN_RADIUS ^ 2,
        TREE_CLASS == "Coniferous" & Z_POS <= 1100  ~ exp (1.514295763 + 0.08884223 * TREE_HEIGHT + 0.02451053 * CROWN_RADIUS + -0.00081988 * TREE_HEIGHT ^ 2 + 0.00238753 * CROWN_RADIUS ^ 2),
        TREE_CLASS == "Deciduous" ~ exp (1.466743252 + 0.0896038 * TREE_HEIGHT + 0.08079226 * CROWN_RADIUS + -0.00108584 * TREE_HEIGHT ^ 2 + -0.0019256 * CROWN_RADIUS ^ 2)
      ),
      
      #' calculate the timber stock:
      STOCK = case_when(
        TREE_CLASS == "Coniferous" & Z_POS > 1100 ~ exp (-6.141586 + 0.33287 * TREE_HEIGHT + 0.271382 * CROWN_RADIUS + -0.004105 * TREE_HEIGHT ^ 2 + -0.0139 * CROWN_RADIUS ^ 2),
        TREE_CLASS == "Coniferous" & Z_POS <= 1100 ~ exp (-5.389 + 0.25144704 * TREE_HEIGHT + 0.06587771 * CROWN_RADIUS + -0.00232442 * TREE_HEIGHT ^ 2 + 0.00274139 * CROWN_RADIUS ^ 2),
        TREE_CLASS == "Deciduous" ~ exp (-6.1162059 + 0.29604055 * TREE_HEIGHT + 0.19163645 * CROWN_RADIUS + -0.00356909 * TREE_HEIGHT ^ 2 + -0.00523082 * CROWN_RADIUS ^ 2)
      ),
      
      #' calculate the basal area:
      BASAL_AREA = (DBH / 200) ^ 2 * pi,
      
      .after = CROWN_VOLUME) %>% 
    
    #' round all metrics to two digits:
    mutate(across(c(CROWN_AREA, CROWN_RADIUS, DBH, STOCK, BASAL_AREA), ~round(.x, 2)))
  
  #' export as gpkg:
  st_write(ST_polygons_edit, dsn = paste0(out_dir, gpkg_name), layer = paste0(layername), 
           driver = "GPKG", append = T, quiet = T)
  
  #' remove data from memory to save space:
  rm(ST_polygons)
  rm(ST_polygons_edit)
  gc()
  
}

# Get the list of all layers in the GeoPackage
gpkg_path <- "E:/Single tree polygons 2017/temp/NCUT_polygons_2017_UTM.gpkg"
layer_names <- st_layers(gpkg_path)$name
layer_names <- layer_names[1]
layer_names

pblapply(layer_names, calculate_metrics, out_dir = "E:/Single tree polygons 2017/temp/", gpkg_name = "NCUT_polygons_2017_metrics.gpkg")



## 2. Rasterize the tree type coverage per pixel -----------------------------------------------------------------------



#' load a reference image, that holds the raster cells the calculatios will be based on
reference_image <- rast("D:/10m_reference_image.tif")
reference_image

#' list of all single tree polygon files
files <- list.files("D:/SIngle tree polygons 2017/Projected_UTM/", pattern = "*.gpkg$", recursive = T, full.names = TRUE)
filenames <- substr(basename(files), 1 , nchar(basename(files))-5)

#' function, that iterates through all polygons in list and calculates the coverage percentage per pixel

for (k in 1:length(files)) {
  
  #' read in and preprocess polygons:
  polygon <- read_sf(files[[k]], quiet = T) %>%     #' read in polygons
    st_transform(crs = crs(reference_image)) %>%    #' transform coordinate system to match reference raster
    mutate(VALID = st_is_valid(.)) %>%              #' check individual polygons, if valid
    filter(VALID == T)                              #' remove invalid polygons
  
  #' check for invalid polygons:
  table(polygon$VALID, useNA = "always")
  
  #' crop reference image to polygon extents:
  reference_image_crop <- crop(reference_image, extent(polygon))
  
  #' split polygons for each tree class:
  polygon_per_class <- polygon %>% 
    mutate(TREE_CLASS= factor(TREE_CLASS, levels = unique(TREE_CLASS))) %>%
    group_by(TREE_CLASS) %>% 
    group_split(.keep = T) %>% 
    setNames(unique(polygon$TREE_CLASS))
  
  #' rasterize the polygons for each tree class to get the coverage percentage:
  
  raster_per_class <- list()
  
  for (i in 1:length(polygon_per_class)) {
    poly <- polygon_per_class[[i]]
    raster_per_class[[i]] <- terra::rasterize(x = poly, y = reference_image_crop, cover = T)
  }
  
  #' combine all rasters in stack:
  raster_stack <- rast(raster_per_class)
  names(raster_stack) <- unique(polygon$TREE_CLASS)
  
  #' export tif files:
  terra::writeRaster(x = raster_stack, 
                     filename = paste0("D:/Single tree polygons 2017/Cover Rasters 10m/", filenames[[k]], "_coverage.tif"), 
                     overwrite = T)
  
  #' clean up environment after each iteration to avoid memory constraints:
  rm(c(poly, polygon, polygon_per_class, raster_per_class, raster_stack, reference_image_crop))
  
}



