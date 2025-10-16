## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2025
#' Status: Work in progress 


### Purpose of script ----


### Notes ----


### Required datasets ----


### Required packages ----

library(lidR)
library(bfnpALSprocessor)
library(sf)
library(mapview)
library(future)
library(tidyverse)
library(pbapply)
library(stringr)
library(RCSF)
library(yardstick)



## 1. Retile the data --------------------------------------------------------------------------------------------------


#' Catalog is retiled to assure filename consistency between the different datasets
ctg <- readALSLAScatalog("G:/01_point_clouds/ALS 2019-2020/full_extent_original")
crs(ctg) <- "EPSG:25832"

las_check(ctg)
plot(ctg)

ctg_retiled <- catalog_retile_template(lascatalog = ctg, output_path = "G:/01_point_clouds/ALS 2019-2020/full_extent_retiled")


## 2. Normalization ----------------------------------------------------------------------------------------------------

ctg_retiled <- readALScatalog("G:/01_point_clouds/ALS 2019-2020/full_extent_retiled")
ctg_retiled

crs(ctg_retiled) <- "epsg:25832"
plot(ctg_retiled, mapview = T)

#' data gets normalized based on the official state DTM that was derived from this specific pointcloud: 
ctg_normalized <- catalog_normalize(ctg_retiled, algorithm = "dtm", dtm_path = "G:/02_dtms/ALS 2019-2020/DTM1_Bayern_NPV_5km.tif", 
                                    output_path = "G:/01_point_clouds/ALS 2019-2020/full_extent_normalized", parallel = T, n_cores = 12)

## 3. Finalizing dataset -----------------------------------------------------------------------------------------------

#' generate footprint polygons:
ctg_polygons <- catalog_to_polygons(ctg_retiled)
ctg_polygons

#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = "G:/ALS 2007/ALS_tiles.gpkg", layer = "ALS_2007", append = T)



## 3. Clip AOIs --------------------------------------------------------------------------------------------------------


#' read AOIs:
test_areas <- st_read("G:/misc/test_areas.gpkg", layer = "AOIs_UTM")
mapview(test_areas)

ctg_AOIs <- catalog_clip_polygons(ctg_normalized, input_epsg = "EPSG:25832", output_path = "G:/ALS 2007/pointclouds_test_AOIs/normalized",
                                  filename_convention = "AOI_{name}", polygons = test_areas)

