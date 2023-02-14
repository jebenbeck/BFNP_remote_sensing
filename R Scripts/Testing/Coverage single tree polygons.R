library(sf)
library(terra)
library(dplyr)
library(raster)
library(mapview)

reference_image <- rast("D:/10m_reference_image.tif")
reference_image

files <- list.files("D:/Einzelbaumpolygone 2017/Projected/", pattern = "*.gpkg$", recursive = T, full.names = TRUE)
filenames <- substr(basename(files), 1 , nchar(basename(files))-5)


#' convert to raster:

for (k in 1:length(files)) {

  k <- 10
  
  #' read in and preprocess polygons:
  polygon <- read_sf(files[[k]], quiet = T) %>%     #' read in polygons
    st_transform(crs = crs(reference_image)) %>%    #' transform coordinate system to match reference raster
    mutate(VALID = st_is_valid(.)) %>%              #' check individual polygons, if valid
    filter(VALID == T)                              #' remove invalid polygons
    
  #' check for invalid polygons:
  table(polygon$VALID, useNA = "always")
  
  #' crop reference image to polygon extents:
  reference_image_crop <- crop(reference_image, extent(polygon))
  #mapview(raster(reference_image_crop))
  
  #' split polygons for each tree class:
  polygon_per_class <- polygon %>% 
    mutate(TREE_CLASS= factor(TREE_CLASS, levels = base::sort(unique(TREE_CLASS)))) %>%
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
  #plot(raster_stack)
  
  #' export tif files:
  terra::writeRaster(x = raster_stack, filename = paste0("D:/Einzelbaumpolygone 2017/Cover Rasters/", filenames[[k]], "_coverage.tif"), overwrite = T)
  
  #' clean up environment:
  rm(poly)
  rm(polygon)
  rm(polygon_per_class)
  rm(raster_per_class)
  rm(raster_stack)
  rm(reference_image_crop)

}
