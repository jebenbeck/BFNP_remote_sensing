## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2024
#' Status: Work in progress 


### Purpose of script ----

#' during the laserscanning campaign along the concentric inventory circle plots, ground reference points were recorded to georeference the data
#' these were measured using azimut, distance and angle of each plot regarding to the center plot
#' the center plot was recorded using the internal tablet gps and external gnss data (raw format)

#' This script calculates the true UTM32 coordinates from the original measurements and exports them as csv and gpkg.
 

### Notes ----


### Required datasets ----


### Required packages ----

require(readxl)
require(tidyverse)
require(units)
require(ggplot2)
library(plotly)

### Required functions and scripts ----


### Set working directories ----




## 1. Calculate relative coordinates -----------------------------------------------------------------------------------

input_path <- "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Collect Daten/collect-xlsx-data-export-laserscanning_waldinventur-ENTRY-2024-09-20T09_56_30/"

plot_data <- read_xlsx(paste0(input_path, "aufnahme_terrestrischer_laserscanner.xlsx"))
gcps <- read_xlsx(paste0(input_path, "gcps.xlsx"))
inventory_plots <- "path/to/best/inv_centerpoints.gpkg"


### Convert the GCP coordinates ----

#' Convert true distance to horizontal distance:

deg_2_rad <- function(angle_deg) {(angle_deg * pi) / (180)}

sd_2_hd <- function(angle_deg, dist_s){
  angle_rad <- deg2rad(angle_deg)
  dist_h <- (dist_s * cos(angle_rad))
  return(dist_h)
}

gcps$gcp_dist_h <- sd_2_hd(gcps$gcp_winkel, gcps$gcp_dist)
View(gcps)

#' convert polar coordinates to relative XYZ-Coordinates:
gcps$X_rel <- (gcps$gcp_dist_h/100)*sin(deg_2_rad(gcps$gcp_azimut))
gcps$Y_rel <- (gcps$gcp_dist_h/100)*cos(deg_2_rad(gcps$gcp_azimut))
gcps$Z_rel <- (gcps$gcp_dist_h/100)*tan(deg_2_rad(gcps$gcp_winkel))
View(gcps)


#' vusialize:
plot_ly(x=gcps$X_rel, y=gcps$Y_rel, z=gcps$Z_rel, type="scatter3d", mode="markers")


### Convert to absolute geographic coordinates:

# join the center point infor to each plot:
left_join(gcps, plot_data, by = c())



