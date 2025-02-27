library(sf)
library(tidyverse)
library(mapview)
library(ggplot2)

#' Accurracy assessment 

#' load AOIs:
AOIs <- st_read("D:/Reproject ALS Data test/AOIs.gpkg", layer = "AOIs_UTM")
AOIs$name

#' preprocessing the GCPs derived from ALS point clouds in cloudcompare:

file_list <- list.files("H:/Reproject ALS Data test/ALS data/ALS 2017/GCPs", pattern = "\\.txt$", full.names = T)

# Read all files into a list using read.table
data <- lapply(file_list, read.table, header = F, sep = ",") %>% 
  bind_rows() %>% 
  rename_with(~ c("Description", "X", "Y", "Z")) %>% 
  st_as_sf(coords = c("X", "Y"), crs = "EPSG:25832", remove = F) %>% 
  st_join(AOIs["name"]) %>% 
  rename(AOI = name) %>% 
  group_by(AOI) %>% 
  mutate(ID = paste0(substr(AOI, 1, 3), "_", row_number()),  .before = everything()) %>% 
  ungroup()
data

#' export to disk:
st_write(data, "D:/Reproject ALS Data test/ALS data/ALS 2019/GCPs/GCPs_ALS_2017.gpkg")

#' compare:

GCPs_ref <- st_read("D:/Reproject ALS Data test/ALS data/ALS 2019/GCPs/GCPs_ALS_2019.gpkg")
GCPs_2017 <- st_read("D:/Reproject ALS Data test/ALS data/ALS 2017/GCPs/GCPs_ALS_2017.gpkg")

#' calculate indices describing the nearest feature:
nearest_indices <- st_nearest_feature(GCPs_ref, GCPs_2017)

# Add corresponding matches from sf2 to sf1
GCPs_ref$matchedID <- GCPs_2017$ID[nearest_indices]

# Join attributes
GCPs_2017_ref <- left_join(as.data.frame(GCPs_2017), as.data.frame(GCPs_ref), by = c("ID" = "matchedID"), suffix = c("", "_ref"), keep = F) %>% 
  mutate(deltaX = X - X_ref, 
         deltaY = Y - Y_ref,
         deltaZ = Z - Z_ref) %>% 
  filter(abs(deltaX) < 5) %>%        #' filter points that are less the 5 m apart to their match
  select(ID, Description, AOI, X, Y, Z, deltaX, deltaY, deltaZ, geom) %>% 
  st_as_sf() 

#' re-format table for better visualization:
test <- GCPs_2017_ref %>% pivot_longer(cols = c(deltaX, deltaY, deltaZ), names_to = "Direction", values_to = "Delta")

#' visualize:
ggplot(data = test, aes(y = Delta, x = AOI)) +
  geom_boxplot() + 
  facet_wrap(vars(Direction), nrow = 3, ncol = 1, shrink = F) + 
  theme_bw() +
  theme(
    axis.title.x = element_blank()
  )
