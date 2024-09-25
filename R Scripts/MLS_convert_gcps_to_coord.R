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
require(ggplot2)
require(plotly)
require(sf)
require(mapview)


### Required functions and scripts ----

#' function to convert degree to radians:
deg_2_rad <- function(angle_deg) {(angle_deg * pi) / (180)}

#' function to convert true distance to horizontal distance
sd_2_hd <- function(angle_deg, dist_s){
  angle_rad <- deg_2_rad(angle_deg)
  dist_h <- (dist_s * cos(angle_rad))
  return(dist_h)
}


### Import data ----

input_path <- "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Collect Daten/Export/"

plot_data <- read_xlsx(paste0(input_path, "aufnahme_terrestrischer_laserscanner.xlsx"))
View(plot_data)

gcps <- read_xlsx(paste0(input_path, "gcps.xlsx")) %>% 
  select(-contains("unit_name")) %>% 
  rename( "plot_id" = "aufnahme_terrestrischer_laserscanner_plot_id")

gcps

### Convert the GCP coordinates ----

#' calculate the horizontal distance:
gcps$gcp_dist_h <- sd_2_hd(gcps$gcp_winkel, gcps$gcp_dist)

#' convert polar coordinates to relative XYZ-Coordinates:
gcps$X_rel <- (gcps$gcp_dist_h/100)*sin(deg_2_rad(gcps$gcp_azimut))
gcps$Y_rel <- (gcps$gcp_dist_h/100)*cos(deg_2_rad(gcps$gcp_azimut))
gcps$Z_rel <- (gcps$gcp_dist_h/100)*tan(deg_2_rad(gcps$gcp_winkel))

#' visualize relative coordinates for checking:
plot_ly(data = gcps, x = ~X_rel, y = ~Y_rel, z = ~Z_rel, type="scatter3d", mode="markers", color = gcps$gcp_richtung, 
        text = ~paste("ID:", plot_id, "<br>", 
                      "X:", X_rel, "<br>", 
                      "Y:", Y_rel, "<br>", 
                      "Z:", Z_rel),
        hoverinfo = "text") 

View(gcps)

#' calculate absolute UTM coordinates by using the center point coordinates:
gcps_koord <- gcps %>% 
  left_join(select(plot_data, c("plot_id",
                                "koordinate_x",
                                "koordinate_y",
                                "koordinate_altitude")), 
            by = "plot_id") %>% 
  mutate(X_UTM = X_rel + koordinate_x,
         Y_UTM = Y_rel + koordinate_y,
         Z_DHHN16 = Z_rel + koordinate_altitude)

#' select center coordinates and append them to the gcps:
plot_data_coord <- select(plot_data, c("plot_id",
                                       "koordinate_x",
                                       "koordinate_y",
                                       "koordinate_altitude")) %>% 
  rename("X_UTM" = "koordinate_x",
         "Y_UTM" = "koordinate_y",
         "Z_DHHN16" = "koordinate_altitude") %>% 
  mutate("_gcps_position" = 0,
         "gcp_richtung" = "z")

gcps_coord_full <- coord <- bind_rows(gcps_koord, plot_data_coord) %>% 
  #' name the point data to their real names:
  mutate(Name = paste0(plot_id, "_", gcp_richtung), .before = gcp_azimut) %>% 
  arrange(plot_id, gcp_richtung)
gcps_coord_full


#' convert to spatial data:
sf <- st_as_sf(gcps_coord_full, coords = c("X_UTM", "Y_UTM"), crs = "EPSG:25832", na.fail = F, remove = F)

mapview(sf, label = sf$plot_id, zcol = "gcp_richtung")


### Export data ----

#' export GCPs to gpkg:
st_write(sf, dsn = "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Export/Punkte_V1.gpkg")

#' export GCPs to csv in FARO-specific format:
gcps_koord_faro <- gcps_coord_full %>% 
  select(c(Name, X_UTM, Y_UTM, Z_DHHN16)) %>% 
  drop_na() %>% 
  setNames(c("Name", "X", "Y", "Z"))

write.table(gcps_koord_faro, file = "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Export/Punkte_V1.txt", sep = ",", quote = F, row.names = F)
