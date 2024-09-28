## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2024
#' Status: Work in progress 


### Purpose of script ----

#' during the terrestrial laserscanning campaign along the concentric inventory circle plots, ground control points 
#' were recorded in order to georeference the data
#' these were measured using azimut, distance and angle of each plot regarding to the center plot
#' the center plot was recorded using the internal tablet gps and external gnss data (raw format)

#' This script calculates the X,Y,Z coordinates relative to the center plot and absolute in UTM32
#' The resulting data sets are exported as spatial (Geopackage) and tabular (csv) files for further usage


### Notes ----

#' TO-DO: 
#' - renaming all column names
#' - export in FARO-csv-format (seperated by plot)

### Required datasets ----


### Required packages ----

require(readxl)
require(tidyverse)
require(ggplot2)
require(plotly)
require(sf)
require(mapview)



## 1. Convert the data -------------------------------------------------------------------------------------------------


### Required functions and scripts ----

#' function to convert degree to radians:
deg_2_rad <- function(angle_deg) {(angle_deg * pi) / (180)}

#' function to convert true distance to horizontal distance
sd_2_hd <- function(angle_deg, dist_s){
  angle_rad <- deg_2_rad(angle_deg)
  dist_h <- (dist_s * cos(angle_rad))
  return(dist_h)
}


### Import required datasets ----

#' define path to the data exported by OpenFOris Collect app (excel files)
input_path <- "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Collect Daten/Export/"

#' import the plot portion of the data:
plot_data <- read_xlsx(paste0(input_path, "aufnahme_terrestrischer_laserscanner.xlsx"))

#' import the gcps:
gcps <- read_xlsx(paste0(input_path, "gcps.xlsx")) %>% 
  select(-contains("unit_name")) %>% 
  rename( "plot_id" = "aufnahme_terrestrischer_laserscanner_plot_id")

#' import the coordinates as measured by differential GNSS (external)
plot_coordinates <- plot_data %>% 
  select("plot_id", "koordinate_x", "koordinate_y", "koordinate_altitude") %>% 
  rename("X_UTM_center" = "koordinate_x", 
         "Y_UTM_center" = "koordinate_y",
         "Z_DHHN16_center" = "koordinate_altitude") 


### Pipeline to process the data ----

gcps_coord <- gcps %>%
  #' calculate the horizontal distance:
  mutate(gcp_dist_h = sd_2_hd(gcp_winkel, gcp_dist)) %>%
  #' convert polar coordinates to relative XYZ-Coordinates:
  mutate(X_rel = (gcp_dist_h/100)*sin(deg_2_rad(gcp_azimut)),
         Y_rel = (gcp_dist_h/100)*cos(deg_2_rad(gcp_azimut)),
         Z_rel = (gcp_dist_h/100)*tan(deg_2_rad(gcp_winkel))) %>% 
  #' join relating center point coordinate to each gcp:
  left_join(plot_coordinates, by = "plot_id") %>% 
  #' calculate absolute UTM coordinates by using the center point coordinates:
  mutate(X_UTM = X_rel + X_UTM_center,
         Y_UTM = Y_rel + Y_UTM_center,
         Z_DHHN16 = Z_rel + Z_DHHN16_center) %>% 
  #' remove unneccessary columns:
  select(-c(X_UTM_center, Y_UTM_center, Z_DHHN16_center))

gcps_coord         

#' reformat the center coordinates to be appended to the gcp dataset:
plot_coordinates_formatted <- plot_coordinates %>% 
  rename("X_UTM" = "X_UTM_center",
         "Y_UTM" = "Y_UTM_center",
         "Z_DHHN16" = "Z_DHHN16_center") %>% 
  mutate("_gcps_position" = 0,
         "gcp_richtung" = "z")

#' add the center coordinates to the gcp dataset as additional gcp:
gcps_coord_full <- bind_rows(gcps_coord, plot_coordinates_formatted) %>% 
  #' replace all NAs with 0:
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  #' name each gcp after its' position and plot_id:
  mutate(gcp_name = paste0(plot_id, "_", gcp_richtung), .before = gcp_azimut) %>% 
  #' sort the dataset:
  arrange(plot_id, gcp_richtung)

gcps_coord_full

#' convert all gcps to spatial data:
gcps_coord_full_spatial <- st_as_sf(gcps_coord_full, coords = c("X_UTM", "Y_UTM"), crs = "EPSG:25832", na.fail = F, remove = F)

### Check and visualize the data ----

#' visualize relative coordinates for checking:
plot_ly(data = gcps_coord, x = ~X_rel, y = ~Y_rel, z = ~Z_rel, type="scatter3d", mode="markers", color = gcps$gcp_richtung, 
        text = ~paste("ID:", plot_id, "<br>", 
                      "X:", X_rel, "<br>", 
                      "Y:", Y_rel, "<br>", 
                      "Z:", Z_rel),
        hoverinfo = "text") 

#' visualize spatial dataset:
mapview(gcps_coord_full_spatial, label = gcps_coord_full_spatial$gcp_name, zcol = "gcp_richtung")


### Export data ----

#' export GCPs to gpkg:
st_write(sf, dsn = "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Export/Punkte_V1.gpkg")

#' export GCPs to csv in FARO-specific format:
gcps_koord_faro <- gcps_coord_full %>% 
  select(c(Name, X_UTM, Y_UTM, Z_DHHN16)) %>% 
  drop_na() %>% 
  setNames(c("Name", "X", "Y", "Z"))

write.table(gcps_koord_faro, file = "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Export/Punkte_V1.txt", sep = ",", quote = F, row.names = F)
