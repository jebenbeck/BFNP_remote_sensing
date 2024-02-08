## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2024-02-07
#' Status: Work in progress 


### Purpose of script ----

#' calculate LiDAR metrics from airborne laserscanning data for 157 Bioklim plots in the Bavarian Forest National Park


### Notes ----


### Required datasets ----

#' 1. ALS data: Point cloud data from 2023 ALS campaign on RSDB server
#' 2. Bioklim plots: Shapefile featuring the 157 Bioklim plots as polygons 


### Required packages ----

require(RSDB)
require(sf)
require(terra)
require(dplyr)
require(mapview)


### Required functions and scripts ----


#' provide login credentials:
source("C:/Users/jakob/OneDrive/BFNP/Projects/Forest-Ecosystem-Monitoring/R Scripts/RSDB/RSDB credentials.R")

# Connect to the server:
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials)


### Set working directories ----

setwd("C:/Users/jakob/OneDrive/BFNP/Projects/Other/LiDAR metrics Bioklim/Data/")



## Derive LiDAR metrics for each location ------------------------------------------------------------------------------


#' connect to ALS point cloud database:
ALS_data <- db$pointcloud('ALS_2017-06')

#' load and preprocess the Haselhuhn point data:
Bioklim_plots <- read_sf("Bioklim Plots/Bioklim_2016_157Plots_ETRS_Puffer50m.shp") %>% 
  st_transform(crs = ALS_data$proj4) %>%           #' convert coordinate system to match the ALS data
  as("Spatial")                                    #' convert to spatial polygons

mapview(Bioklim_plots)

#' convert to matrix and set names to perform index calculations:
Bioklim_plots_matrix <- convert_SpatialPolygons_to_named_matrix_list(Bioklim_plots)
names(Bioklim_plots_matrix) <- Bioklim_plots$Plot

Bioklim_plots_matrix


indices_names <- c("Vegetation height mean", "Vegetation height max", "Vegetation height standard deviation", 
  "Penetration rate 1-2m", "Penetration rate 2-5m", "Penetration rate 2-50m")






#'create a list of all indices to be calculated (names according to RSDB terminology):
#indices = c("BE_H_MEAN", "BE_H_MAX", "BE_H_SD", "BE_PR_01", paste0("BE_PR_", sprintf("%02d", 2:49)), "BE_PR_UND")
indices = c(paste0("BE_PR_", sprintf("%02d", 1:5)), paste0("BE_RD_", sprintf("%02d", 1:5)), "BE_PR_UND")
indices

#' calculate LiDAR indices for all polygons on RSDB server:
Bioklim_plots_indices <- ALS_data$indices(areas = Bioklim_plots_matrix, functions = indices)
head(Bioklim_plots_indices)

#' calculate
Bioklim_plots_indices_test <- Bioklim_plots_indices %>% 
  mutate (test = 1-BE_PR_02 + 1-BE_PR_03 + 1-BE_PR_04,
          BE_PR_UND_calc = 1-test) 

head(Bioklim_plots_indices_test)









#' rename index colums:
colnames(Bioklim_plots_indices) <- c("id", indices$Description)
names(Bioklim_plots_indices)

#' join new information to the old table:
Bioklim_plots@data <- base::merge(Bioklim_plots@data, Bioklim_plots_indices, by.x = "Plot", by.y = "id", all = T)
names(Bioklim_plots@data)
head(Bioklim_plots@data)

#' convert Bioklim_plots to sf data:
Bioklim_plots <- st_as_sf(Bioklim_plots)

#' export Bioklim_plots as geopackage to disk:
st_write(Bioklim_plots, "Bioklim_plots_LiDAR_metrics.gpkg", driver = "GPKG")