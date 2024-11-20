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
require(lidR)


### Required functions and scripts ----


#' provide login credentials:
source("C:/Users/jakob/OneDrive/BFNP/Projects/Forest-Ecosystem-Monitoring/R Scripts/RSDB/RSDB credentials.R")

# Connect to the server:
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials)


### Set working directories ----
setwd("C:/Users/jakob/OneDrive/BFNP/Projects/Forest-Ecosystem-Monitoring/R Scripts/LiDAR metrics Bioklim/Data/")



## 1. Load and Preprocess AOIs -----------------------------------------------------------------------------------------


#' load and preprocess the Bíoklim plot areas:
Bioklim_plots <- read_sf("Bioklim Plots/Bioklim_2016_157Plots_ETRS_Puffer50m.shp") %>% 
  st_transform(crs = "EPSG:32632") %>%           #' convert coordinate system to match the ALS data
  as("Spatial")                                    #' convert to spatial polygons

mapview(Bioklim_plots)

#' convert to matrix and set names to perform index calculations:
Bioklim_plots_matrix <- convert_SpatialPolygons_to_named_matrix_list(Bioklim_plots)
names(Bioklim_plots_matrix) <- Bioklim_plots$Plot



## 2. Query pointcloud data --------------------------------------------------------------------------------------------


#' get the extent of one plot as the AOI:
#AOI <- extent(HTO_test_tiles_poly[20,])
AOI <- Bioklim_plots_matrix$T4_64

#' connect to ALS point cloud database:
ALS_data.db <- db$pointcloud('ALS_2017-06')

#' querry the points:
ALS_data.points <- ALS_data.db$points(ext=AOI)
head(ALS_data.points)

#' redefine ScanAngleRank
ALS_data.points$scanAngleRank <- 33

#' convert dataframe to LAS:
ALS_data.las <- RSDB::as.LAS(ALS_data.points, proj4string = crs(ALS_data.db))
ALS_data.las

#' change the projection:
projection(ALS_data.las)<- "EPSG:32632"

#' write to disk:
writeLAS(ALS_data.las, "ALS_2017.las")



## 3. Calculate penetration rates for different levels -----------------------------------------------------------------



calculate_penetration_rate <- function(layer) {
  # Assuming the layer is a LAS object
  # Extract Z coordinate (height) of each point in the layer
  Z_values <- layer@data$Z
  
  # Define the lower threshold of the layer
  lower_threshold <- 1  # Define your lower threshold value here
  
  # Count the number of pulses that hit the bottom of the layer
  hits_bottom <- sum(Z_values <= lower_threshold)
  
  # Count the total number of transmitted pulses (total points in the layer)
  total_points <- length(Z_values)
  
  # Calculate penetration rate
  penetration_rate <- (hits_bottom / total_points) * 100
  
  return(penetration_rate)
}



# Segment the point cloud into layers (e.g., based on height)
# You can use different segmentation algorithms or filters depending on your data
# Here's a simple example using a height-based filter
layers <- catalog_apply(las, function(chunk) {
  lasfilter = filter_poi(chunk, Z > min_height & Z < max_height)
})

# Calculate penetration rates for each layer
penetration_rates <- sapply(layers, function(layer) {
  # Calculate penetration rate metric for the layer
  # This could be based on the density of points, vegetation indices, etc.
  # Replace this with your specific metric calculation
  penetration_rate <- your_calculation_function(layer)
  return(penetration_rate)
})

# Print or further analyze penetration rates
print(penetration_rates)













## 1. Derive LiDAR metrics for each location ------------------------------------------------------------------------------

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