## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2025
#' Status: Work in progress 


### Purpose of script ----


### Notes ----


### Required datasets ----


### Required packages ----

library(lidR)
library(sf)
library(mapview)
library(future)
library(tidyverse)
library(stringr)

### Required functions and scripts ----

source("ALS data preparation/Processing functions master.R")

### Set working directories ----

#' set wd of drive where the ALS 2017 database is stored. Must be changed when switching PCs

path_drive <- "D:/"


## 1. Reproject to UTM32 -----------------------------------------------------------------------------------------------


#' read LasCatalog:
ctg <- readALSLAScatalog(paste0(path_drive, "Reproject ALS Data test/LiDAR GK/Originaldaten_subset"))

#' check LAScatalog validity:
ctg
summary(ctg)
las_check(ctg)
plot(ctg)

#' apply the transformation:
ctg_UTM32 <- reproject_lascatalog(lascatalog = ctg,
                                  input_epsg = "EPSG:31468",
                                  output_epsg = "EPSG:25832", 
                                  output_path = paste0(path_drive, "ALS 2017/2_pointclouds_UTM"))

#' check LAScatalog validity:
summary(ctg_UTM32)
las_check(ctg_UTM32)
plot(ctg_UTM32)


# export catalog polygons ----
ctg_polygons <- catalog_to_polygons(ctg_UTM32)

#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = paste0(path_drive, "Tiles.gpkg"), layer = "ALS_2017" , append = T)



## 2. Retile catalog ---------------------------------------------------------------------------------------------------

ctg_UTM32_retiled <- catalog_retiling(lascatalog = ctg_UTM32, output_path = paste0(path_drive, "ALS_2017/3_pointclouds_retiled"))

#' check LAScatalog validity:
ctg_UTM32_retiled
summary(ctg_UTM32_retiled)
las_check(ctg_UTM32_retiled)
plot(ctg_UTM32_retiled, mapview = T)



## 3. Generate footprint polygons --------------------------------------------------------------------------------------

#' read LasCatalog:
ctg_UTM32_retiled <- readALSLAScatalog(paste0(path_drive, "ALS 2017/3_pointclouds_retiled"))

# convert catalog to polygons:
ctg_polygons <- catalog_to_polygons(ctg_UTM32_retiled)

#' calculate statistics on catalog:
ctg_UTM32_stats <- catalog_statistics(ctg_UTM32_retiled, parallel = T, n_cores = 3)

#' merge the data: 
ctg_polygons_stats <- left_join(ctg_polygons, ctg_UTM32_stats) %>% 
  relocate(c(Point.density, Area.covered), .after = Tile.name) %>% 
  relocate(c(Tile.max.X, Tile.min.X, Tile.max.Y, Tile.min.Y), .after = Min.Z)
ctg_polygons_stats

#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = paste0(path_drive, "Tiles.gpkg"), layer = "ALS_2017" , append = T)



## 4. Cut AOIs from catalog --------------------------------------------------------------------------------------------



#' read LasCatalog:
ctg_UTM32_retiled <- readALSLAScatalog(paste0(path_drive, "ALS 2017/3_pointclouds_retiled"))

#' read AOIs:
AOIs <- st_read("D:/Reproject ALS Data test/AOIs.gpkg", layer = "AOIs_UTM")
mapview(AOIs)


ctg_UTM32_AOIs <- catalog_clip_polygons(ctg_UTM32_retiled, input_epsg = "EPSG:25832", output_path = "D:/Reproject ALS Data test/ALS data/ALS 2017/AOIs_UTM32",
                      filename_convention = "AOI_{name}", polygons = AOIs)



