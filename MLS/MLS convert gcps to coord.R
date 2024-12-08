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
#' - implement external coordinates from GNSS measurements to calculate UTM32 coordinates

### Required datasets ----


### Required packages ----

require(readxl)
require(tidyverse)
require(ggplot2)
require(plotly)
require(sf)
require(mapview)
require(terra)



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

#' define path to the data exported by OpenForis Collect app (excel files)
input_path <- "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Collect Daten/Export/"

#' import the plot portion of the data:
plot_data <- read_xlsx(paste0(input_path, "aufnahme_terrestrischer_laserscanner.xlsx"))

#' import the gcps:
gcps <- read_xlsx(paste0(input_path, "gcps.xlsx")) %>% 
  select(-contains("unit_name")) %>% 
  rename( "plot_id" = "aufnahme_terrestrischer_laserscanner_plot_id")

#' import the coordinates as measured by differential GNSS (external)
#' just for testing purposes still:

plot_coordinates <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Inventory plots center coordinates/Plots_surveyed_2024-11-19.gpkg") %>% 
  select("plot_id", "X_UTM", "Y_UTM", "Z_DHHN16") %>% 
  rename("X_UTM_center" = "X_UTM", 
         "Y_UTM_center" = "Y_UTM",
         "Z_DHHN16_center" = "Z_DHHN16") 
plot_coordinates  


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

plot_coordinates_formatted

#' add the center coordinates to the gcp dataset as additional gcp:
gcps_coord_full <- bind_rows(gcps_coord, plot_coordinates_formatted) %>% 
  #' replace all NAs with 0:
  mutate(
    across(-c(Z_DHHN16, X_UTM, Y_UTM), ~replace_na(.x, 0))
  ) %>% 
  #' name each gcp after its' position and plot_id:
  mutate(gcp_name = paste0(plot_id, "_", gcp_richtung), .before = gcp_azimut) %>% 
  #' sort the dataset:
  arrange(plot_id, gcp_richtung)

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
st_write(gcps_coord_full_spatial, dsn = paste0("C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/GCPs/GCPs_", Sys.Date(), ".gpkg"), append = F)

#' export all center plots to gpkg:
st_write(filter(gcps_coord_full_spatial, gcp_richtung == "z"), 
         dsn = paste0("C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/Plots_surveyed_", Sys.Date(), ".gpkg"), append = F)

gcps_coord_full

#' export relative and absolute GCPs to csv in FARO-specific format (csv) 
gcps_coord_faro <- gcps_coord_full %>% 
  group_by(plot_id) %>% 
  select(c(gcp_name, X_UTM, Y_UTM, Z_DHHN16, X_rel, Y_rel, Z_rel)) %>% 
  #' split into individual data frames per plot:
  group_split(.keep = F) %>% 
  #' rename the seperate files:
  setNames(gcps_coord_full %>% 
             group_by(plot_id) %>% 
             group_keys() %>% 
             pull(plot_id))

#' export loop:
outdir <- "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/GCPs/"

for (i in 1:length(gcps_coord_faro)) {
  #' absolute coordinates
  write.csv(select(gcps_coord_faro[[i]], c("gcp_name", "X_UTM", "Y_UTM", "Z_DHHN16")), 
            file = paste0(outdir,"absolut/GCPs_absolut_", names(gcps_coord_faro[i]), ".csv"), row.names = F, append = F)
  #' relative coordinates
  write.csv(select(gcps_coord_faro[[i]], c("gcp_name", "X_rel", "Y_rel", "Z_rel")), 
            file = paste0(outdir,"relativ/GCPs_relativ_", names(gcps_coord_faro[i]), ".csv"), row.names = F, append = F)
}



## 2. Representation analysis  -----------------------------------------------------------------------------------------


### import data for strata ----

#' 1) the differenz altitudinal zones
altitudinal_zones <- st_read(dsn = "H:/Basisdaten/Höhenstufen/Höhenstufen.gpkg") %>% 
  rename(Höhenlage = Stufe)

#' 2) the different management zones
zonation <- st_read(dsn = "H:/Basisdaten/Zonierung/Zonierung.gpkg")


### Calculate representativeness of the MLS plots ----

#' overlay the strata with the MLS plots:
gcps_coord_full_spatial_strata <- gcps_coord_full_spatial %>% 
  filter(gcp_richtung == "z") %>% 
  st_intersection(., zonation) %>% 
  st_intersection(., altitudinal_zones) %>% 
  select(plot_id, Zone, Höhenlage) %>% 
  st_drop_geometry()

#' by zone
class_percentages_MLS_Zone <- gcps_coord_full_spatial_strata %>%
  group_by(Zone) %>%
  summarise(
    count = n(),
    percentage = (n() / nrow(gcps_coord_full_spatial_strata)) * 100
  ) %>% 
  mutate("Typ" = "MLS")

#' by altitude:
class_percentages_MLS_Altitude <- gcps_coord_full_spatial_strata %>% 
  group_by(Höhenlage) %>%
  summarise(
    count = n(),
    percentage = (n() / nrow(gcps_coord_full_spatial_strata)) * 100
  ) %>% 
  mutate("Typ" = "MLS")


### Calculate representativeness of full strata ----

#' based on the zonation:
class_percentages_total_zone <- zonation %>% 
  select(Zone, Flächenpr) %>% 
  st_drop_geometry() %>% 
  rename(percentage = Flächenpr) %>% 
  mutate("Typ" = "total")

class_percentages_total_zone

#' based on the altitudinal zones:
class_percentages_total_altitude <- altitudinal_zones %>%
  mutate("area" = as.numeric(st_area(.))) %>% 
  st_drop_geometry() %>% 
  group_by(Höhenlage) %>% 
  summarise(area = sum(area)) %>% 
  ungroup() %>% 
  mutate(total_area = sum(area),
         percentage = (area/total_area)*100,
         "Typ" = "total") %>% 
  select(Höhenlage, percentage, Typ)
  
class_percentages_total_altitude


### Visualize the result ----

#' combine the datasets:
class_percentages_zone <- bind_rows(class_percentages_MLS_Zone, class_percentages_total_zone)
class_percentages_zone

class_percentages_altitude <- bind_rows(class_percentages_MLS_Altitude, class_percentages_total_altitude)
class_percentages_altitude

#' plot the representativeness based on the zonation:
class_percentages_zone_plot <- ggplot(data = class_percentages_zone, aes(x = Zone, y = percentage, fill = Typ)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5)

ggsave(filename = "Representativeness_Zones.png", 
       plot = class_percentages_zone_plot, 
       path = "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/",
       device = "png", 
       width = 300, height = 200, units = "mm", dpi = 200)

#' plot the representativeness based on the altitudinal levels:
class_percentages_altitude_plot <- ggplot(data = class_percentages_altitude, aes(x = Höhenlage, y = percentage, fill = Typ)) + 
  geom_col(position = "dodge") + 
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5)

ggsave(filename = "Representativeness_Altitudes.png", 
       plot = class_percentages_altitude_plot, 
       path = "C:/Users/jakob/OneDrive/BFNP/Data/Laserscanner Waldinventur/",
       device = "png", 
       width = 300, height = 200, units = "mm", dpi = 200)

