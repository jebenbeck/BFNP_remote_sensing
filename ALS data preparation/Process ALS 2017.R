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
ctg_dtm <- catalog_dtm(ctg, output_path = "D:/5_1_dtms_2", filename_convention = "{ORIGINALFILENAME}_dtm", 
                       parallel = T, n_cores = 4)

plot(ctg_dtm)




## 8. Normalization ----------------------------------------------------------------------------------------------------


#' read in lascatalog:
ctg <- readALSLAScatalog("D:/4_pointclouds_filtered")

ctg
plot(ctg)
las_check(ctg)

#' perform the normalization with dtm:
ctg_normalized_dtm <- catalog_normalize_dtm(ctg, dtm_path = "D:/dtm_mosaic.tif", output_path = "D:/6_pointclouds_normalized", "{ORIGINALFILENAME}", parallel = F, n_cores = 1)


ctg <- readALSLAScatalog("D:/7_subset_npbw")
las_check(ctg)
plot(ctg)


# convert catalog to polygons:
ctg_polygons <- catalog_to_polygons(ctg)

#' calculate statistics on catalog:
ctg_stats <- catalog_statistics(ctg, parallel = T, n_cores = 3)

#' merge the data: 
ctg_polygons_stats <- left_join(ctg_polygons, ctg_stats) %>% 
  relocate(c(Point.density, Area.covered), .after = Tile.name) %>% 
  relocate(c(Tile.max.X, Tile.min.X, Tile.max.Y, Tile.min.Y), .after = Min.Z)
ctg_polygons_stats

#' export polygon file to geopackage:
st_write(ctg_polygons_stats, dsn = paste0("D:/7_subset_npbw/", "Tiles.gpkg"), layer = "ALS_2017" , append = T)



## 9. Clip to reference areas ------------------------------------------------------------------------------------------

#' read AOIs:
AOIs <- st_read("H:/Waldstruktur Bodendaten/Dauerbeobachtungsflächen/HTO Referenzflächen/Geodaten/Referenzflächen nach Layern stand 2023-10-27.gpkg", layer = "Transekt") %>% 
  st_buffer(50)
mapview(AOIs)
AOIs

# 3. Create a "touch" graph: which polygons intersect which
# st_intersects returns a list where each element contains indices of intersecting polygons
touch_list <- st_intersects(AOIs)

# 4. Turn the list into groups (i.e., components of the touch graph)
# This uses connected components to group overlapping polygons
library(igraph)

# Build a graph
g <- graph_from_adj_list(touch_list, mode = "all")
components <- components(g)$membership
components

# 5. Add group IDs to the sf object
AOIs$group_id <- components
View(AOIs)

# 6. Union polygons within each group
AOIs_grouped <- AOIs %>%
  group_by(group_id) %>%
  summarise(geom = st_union(geom), .groups = "drop")

mapview(AOIs_grouped)


#' read LasCatalog:
ctg_UTM32_retiled <- readALSLAScatalog("H:/ALS 2017/4_pointclouds_filtered")
plot(ctg_UTM32_retiled, mapview = T)

ctg_UTM32_AOIs <- catalog_clip_polygons(ctg_UTM32_retiled, input_epsg = "EPSG:25832", output_path = "H:/ALS 2017/Reference_areas",
                                        filename_convention = "AOI_{ID_plot}", polygons = AOIs_subset)