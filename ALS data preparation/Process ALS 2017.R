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
                                  output_path = paste0(path_drive, "Reproject ALS Data test/LiDAR UTM/test3"))

#' check LAScatalog validity:
summary(ctg_UTM32)
las_check(ctg_UTM32)
plot(ctg_UTM32)


### Export catalog polygons ----

#' Make the polygons:
ctg_polygons <- catalog_to_polygons(ctg_UTM32)
ctg_polygons

#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = paste0(path_drive, "Tiles.gpkg"), layer = "ALS_2017" , append = T)


### Calculate statistics ----

test <- catalog_statistics(ctg_UTM32)
test$Tile.name
ctg_polygons$Tile.name

#' merge the two datasets: 


## 2. Retile catalog ---------------------------------------------------------------------------------------------------

ctg_UTM32_retiled <- catalog_retiling(lascatalog = ctg_UTM32, output_path = paste0(path_drive, "Reproject ALS Data test/LiDAR UTM/test_retile"))
