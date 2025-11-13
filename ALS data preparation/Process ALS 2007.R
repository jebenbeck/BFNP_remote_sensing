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
ctg <- readALSLAScatalog("G:/ALS 2007/pointclouds_full_original")
ctg
las_check(ctg)
plot(ctg, mapview = T)

ctg_retiled <- catalog_retile_template(lascatalog = ctg, output_path = "G:/ALS 2007/pointclouds_full_retiled")
ctg_retiled <- readALScatalog("G:/ALS 2007/pointclouds_full_retiled")

#' generate footprint polygons:
ctg_polygons <- catalog_to_polygons(ctg_retiled)
ctg_polygons

#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = "G:/ALS 2007/ALS_tiles.gpkg", layer = "ALS_2007", append = T)


## 2. Normalize --------------------------------------------------------------------------------------------------------

ctg_normalized <- catalog_normalize(ctg_retiled, algorithm = "dtm", dtm_path = "G:/ALS 2007/dgm1_utm.tif", 
                                    output_path = "G:/ALS 2007/pointclouds_full_normalized", parallel = T, n_cores = 12)


## 3. Clip AOIs --------------------------------------------------------------------------------------------------------


#' read AOIs:
test_areas <- st_read("G:/misc/test_areas.gpkg", layer = "AOIs_UTM")
mapview(test_areas)

ctg_AOIs <- catalog_clip_polygons(ctg_normalized, input_epsg = "EPSG:25832", output_path = "G:/ALS 2007/pointclouds_test_AOIs/normalized",
                                        filename_convention = "AOI_{name}", polygons = test_areas)


## 3. Filter outliers --------------------------------------------------------------------------------------------------

ctg_filtered <- catalog_filter(ctg_normalized, filter_mode = "remove", output_path = "G:/ALS 2007/pointclouds_full_filtered",
                               parallel = T, n_cores = 12)


ctg_AOIs <- catalog_clip_polygons(ctg_filtered, input_epsg = "EPSG:25832", output_path = "G:/ALS 2007/pointclouds_test_AOIs/filtered_2",
                                  filename_convention = "AOI_{name}", polygons = test_areas)


## 4. Export footprint polygons ----------------------------------------------------------------------------------------

ctg <- readALScatalog("I:/01_point_clouds/ALS 2008-2009/full_extent_final")

#' calculate statistics on catalog:
ctg_stats <- catalog_statistics(ctg, parallel = T, n_cores = 18, spatial = T)

#' export polygon file to geopackage:
st_write(ctg_stats, dsn = "I:/misc/ALS_tiles.gpkg", layer = "ALS 2008-2009", append = T)
