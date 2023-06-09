## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Rieser
#' Last updated: 2023
#' Status: Work in progress 


### Purpose of script ----


### Notes ----


### Required datasets ----


### Required packages ----

require(tidyverse)
require(parallel)
library(doParallel)
library(foreach)
require(sf)
require(terra)
require(dplyr)
require(raster)
require(mapview)
require(units)


### Required functions and scripts ----


### Set working directories ----

indir <- "F:/Single tree polygons 2017/Projected_UTM"
outdir <- "F:/Single tree polygons 2017/Volume/"




## 1. Calculate single tree metrics ------------------------------------------------------------------------------------


#' list of all polygons:
ST_files_list <- list.files(indir, pattern = ".*.gpkg$", recursive = T, full.names = TRUE)


#' set up parallel computation:
no_cores <- 8   #' number of cores
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)

#' apply processing chain for each file:
foreach(i = 1:length(ST_files_list), .packages = c("sf", "tidyverse", "units")) %dopar% {
#for(i in 1:length(ST_files_list_subset)) {

    #' select respective file
  ST_file <- ST_files_list[[i]]
  
  #' parse filename:
  filename <- tools::file_path_sans_ext(basename(ST_file))
  
  #' read in the polygons:
  ST_polygons <- read_sf(ST_file, as_tibble = F, quiet = T) #%>% 
    #mutate(VALID = st_is_valid(.)) %>%              #' check individual polygons, if valid
    #filter(VALID == T)                              #' remove invalid polygons
  
  #' Process the data:
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
      
      .after = CROWN_VOLUME) 
  
  #' export as gpkg:
  st_write(ST_polygons_edit, dsn = paste0(outdir, filename, ".gpkg"), driver = "GPKG", append = F)
  
  #' export as csv:
  write.csv2(ST_polygons_edit, file = paste0(outdir, "Tables/", filename, ".csv"))
  
  #' remove data from memory to save space:
  rm(ST_polygons)
  rm(ST_polygons_edit)

}

stopCluster(cl)




## 2. Rasterize metrics ------------------------------------------------------------------------------------------------



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




## 3. Mosaic rasterized metrics ----------------------------------------------------------------------------------------













