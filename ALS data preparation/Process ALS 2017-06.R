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
library(terra)
library(mapview)
library(future)
library(tidyverse)
library(stringr)
library(ggplot2)
library(yardstick)


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


# export catalog polygons:
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
ctg_UTM32_retiled <- readALSLAScatalog("F:/ALS 2017/3_pointclouds_retiled")

#' read AOIs:
AOIs <- st_read("F:/Reproject ALS Data test/AOIs.gpkg", layer = "AOIs_UTM")
mapview(AOIs)

ctg_UTM32_AOIs <- catalog_clip_polygons(ctg_UTM32_retiled, input_epsg = "EPSG:25832", output_path = "F:/ALS 2017/test_AOIs",
                      filename_convention = "AOI_{name}", polygons = AOIs)



## 5. Accurracy evaluation ---------------------------------------------------------------------------------------------


#' load AOIs:
AOIs <- st_read("D:/Reproject ALS Data test/AOIs.gpkg", layer = "AOIs_UTM")

#' prepare the GCPs as generated using CloudCompare:
GCPs_2017 <- prepare_GCPs(input_path = "D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs",
                     output_path = "D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs",
                     filename = "GCPs_ALS_2017", polygons = AOIs)


#' merge the GCPs with the reference GCPs:
GCPs_ref <- st_read("D:/Reproject ALS Data test/ALS data/ALS 2019/GCPs/GCPs_ALS_2019.gpkg")
GCPs_2017 <- st_read("D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs/GCPs_ALS_2017.gpkg")

GCPs_2017_ref <- merge_GCPs(GCPs_2017, GCPs_ref, export = T, filename = "GCPs_ALS_2017_ref", output_path = "D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs")

#' generate boxplots:
GCPs_2017_ref <- st_read("D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs/GCPs_ALS_2017_ref.gpkg")

aa_create_boxplots(gcp_data = GCPs_2017_ref, export = T, filename = "Difference_GCPs_ALS_2017",
                   output_path = "D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs")

#' calculate metrics:
metrics <- aa_metrics(GCPs_2017_ref, export = T, filename = "Metrics_ALS_2017", output_path = "D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs")
metrics



## 6. Outlier filtering ------------------------------------------------------------------------------------------------


#' read in lascatalog:
ctg <- readALSLAScatalog("D:/3_pointclouds_retiled")
ctg

#' perform filtering::
ctg_filtered <- catalog_filter(ctg, filter_mode = "filter", "D:/4_pointclouds_filtered", "{ORIGINALFILENAME}", parallel = F, n_cores = 3)



## 7. DTM creation -----------------------------------------------------------------------------------------------------


#' read in lascatalog:
ctg <- readALSLAScatalog("D:/4_pointclouds_filtered/to-do")
las_check(ctg)
plot(ctg)

#' perform the dtm creation:
ctg_dtm <- catalog_dtm(ctg, output_path = "D:/test", filename_convention = "{ORIGINALFILENAME}_dtm", mosaic_result = T,
                       mosaic_name = "Mosaic", parallel = T, n_cores = 3)

plot(ctg_dtm)




## 8. Normalization ----------------------------------------------------------------------------------------------------


ctg <- readALSLAScatalog("G:/ALS 2017-06/pointclouds_full")
plot(ctg)

ctg_normalized <- catalog_normalize(lascatalog = ctg, algorithm = "dtm", dtm_path = "G:/misc/DTM1_combined_17_19_23.tif", output_path = "G:/ALS 2017-06/pointclouds_full_normalized",
                                    parallel = T, n_cores = 6)

#' calculate statistics on catalog:
ctg_stats <- catalog_statistics(ctg, parallel = T, n_cores = 18, spatial = T)
ctg_stats

#' export polygon file to geopackage:
st_write(ctg_stats, dsn = "G:/misc/ALS_tiles.gpkg", layer = "ALS_2017-06", append = T)



## 9. Clip to reference areas ------------------------------------------------------------------------------------------

#' read AOIs:
Reference_areas <- st_read("E:/Daten für Hetzner-Server/ground_reference.gpkg", layer = "transects_buffered_merged")
mapview(Reference_areas)
Reference_areas


#' read LasCatalog:
ctg_UTM32_retiled <- readALSLAScatalog("E:/ALS 2017/4_pointclouds_filtered")
plot(ctg_UTM32_retiled, mapview = T)

ctg_UTM32_AOIs <- catalog_clip_polygons(ctg_UTM32_retiled, input_epsg = "EPSG:25832", output_path = "E:/ALS 2017/Reference_areas",
                                        filename_convention = "reference_area_{transect_id}_{transect_name}", polygons = Reference_areas)
