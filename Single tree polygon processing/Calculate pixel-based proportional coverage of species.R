## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Rieser
#' Last updated: 2023-02-15
#' Status: Work in progress 


### Purpose of script ----

#' Calculates pixel-based proportional coverage of tree species based on the single tree polygons based on tiles
#' Afterwards, the tiles are mosaiced to have the full extent in a single raster


### Required datasets ----

#' - a raster that holds the cells for the pixel based calculations and has information on where the ALS campaign 
#'   actually acquired data
#' - geolocated and tiled single tree polygons


### Required packages ----

require(sf)
require(terra)
require(dplyr)
require(raster)
require(mapview)



## 1. Calculate coverage for each tile ---------------------------------------------------------------------------------


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



## 2. Mosaic the raster tiles ------------------------------------------------------------------------------------------


#' load in list of rasters:
raster_files <- list.files("D:/Einzelbaumpolygone 2017/Cover Rasters/", pattern = "*.tif$", recursive = T, full.names = TRUE)
filenames <- substr(basename(raster_files), 1 , nchar(basename(raster_files))-4)

#' load in rasters:
raster_list <- lapply(raster_files, rast)

#' reorder bands:
raster_list_ordered <- list()
for (i in 1:length(raster_list)) {
  raster_list_ordered[[i]] <- raster_list[[i]][[names(raster_list[[1]])]]
}

#' convert to a spatial raster collection:
raster_sprc <- sprc(raster_list_ordered)

#' make mosaic from raster collection:
raster_mosaic <- mosaic(raster_sprc, fun = "sum")

#' set all values >1 to 1:
raster_mosaic$decid[raster_mosaic$decid>=1] <- 1
raster_mosaic$conif[raster_mosaic$conif>=1] <- 1
raster_mosaic$deadw[raster_mosaic$deadw>=1] <- 1
raster_mosaic$snag[raster_mosaic$snag>=1] <- 1

#' look at the data:
plot(raster_mosaic)

#' change raster band names:
names(raster_mosaic) <- c("coverage_deciduous", "coverage_coniferous", "coverage_deadwood", "coverage_snag")

#' export raster as tif:
writeRaster(x = raster_mosaic, filename = "Single tree polygons 2017/Cover Rasters/Mosaic/Coverage_tree_types.tif", overwrite = T)



## 3. Clip the mosaic to locations, where data was available -----------------------------------------------------------


#' load in the mosaiced raster which holds NA values for all empty cells:
coverage_raster <- rast("D:/Single tree polygons 2017/Cover Rasters/Mosaic/Coverage_tree_types.tif")

#' change NA values to 0, as the coverage is actually 0 and not NA:
coverage_raster_0 <- subst(coverage_raster, NA, 0)

#' load in the reference raster representing pixels, where the ALS campaign actually acquired data:
reference_image <- rast("D:/10m_reference_raster.tif") %>%
  project(y = crs(coverage_raster))

#' change value range to meet masking requirements:
reference_image[reference_image <= 0] <- NA

#' mask the coverage raster using the reference raster:
coverage_raster_crop <- resample(coverage_raster_0, reference_image)

coverage_raster_masked <- mask(coverage_raster_crop, reference_image)

#' look at the generated data:
plot(coverage_raster_masked)
mapview(raster(coverage_raster_masked$deciduous))

#' export new raster:
terra::writeRaster(x = coverage_raster_masked, 
                   filename = "D:/Single tree polygons 2017/Cover Rasters/Mosaic/Coverage_tree_types.tif", 
                   overwrite = T)