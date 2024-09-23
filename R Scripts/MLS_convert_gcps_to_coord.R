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
library(sf)
library(mapview)

### Required functions and scripts ----


### Set working directories ----





input_path <- "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Collect Daten/collect-xlsx-data-export-laserscanning_waldinventur-ENTRY-2024-09-20T09_56_30/"

plot_data <- read_xlsx(paste0(input_path, "aufnahme_terrestrischer_laserscanner.xlsx"))

gcps <- read_xlsx(paste0(input_path, "gcps.xlsx")) %>% 
  select(-contains("unit_name")) %>% 
  rename("plot_id" = "aufnahme_terrestrischer_laserscanner_plot_id")

gcps

### Convert the GCP coordinates ----

#' Convert true distance to horizontal distance:

deg_2_rad <- function(angle_deg) {(angle_deg * pi) / (180)}

sd_2_hd <- function(angle_deg, dist_s){
  angle_rad <- deg_2_rad(angle_deg)
  dist_h <- (dist_s * cos(angle_rad))
  return(dist_h)
}

gcps$gcp_dist_h <- sd_2_hd(gcps$gcp_winkel, gcps$gcp_dist)

#' convert polar coordinates to relative XYZ-Coordinates:
gcps$X_rel <- (gcps$gcp_dist_h/100)*sin(deg_2_rad(gcps$gcp_azimut))
gcps$Y_rel <- (gcps$gcp_dist_h/100)*cos(deg_2_rad(gcps$gcp_azimut))
gcps$Z_rel <- (gcps$gcp_dist_h/100)*tan(deg_2_rad(gcps$gcp_winkel))


#' visualize coordinates for checking:
plot_ly(x=gcps$X_rel, y=gcps$Y_rel, z=gcps$Z_rel, type="scatter3d", mode="markers", color = gcps$gcp_richtung)


#' calculate absolute coordinates in UTM32: 

#' join center coordinates:
test <- gcps %>% 
  left_join(select(plot_data, c("plot_id",
                                "koordinate_x",
                                "koordinate_y",
                                "koordinate_altitude")), 
            by = "plot_id") %>% 
  mutate(X_UTM = X_rel + koordinate_x,
         Y_UTM = Y_rel + koordinate_y,
         Z_DHHN16 = Z_rel + koordinate_altitude)

x <- st_as_sf(test, coords = c("X_UTM", "Y_UTM"), crs = "EPSG:25832", na.fail = F, remove = F)

mapview(x)
x
# join the center point info to each plot:
left_join(gcps, plot_data, by = c())