library(sf)
library(tidyverse)
library(mapview)
library(ggplot2)

#' Accurracy assessment 

#' load AOIs:
AOIs <- st_read("H:/Reproject ALS Data test/AOIs.gpkg", layer = "AOIs_UTM")
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
st_write(data, "H:/Reproject ALS Data test/ALS data/ALS 2019/GCPs/GCPs_ALS_2017.gpkg")

#' compare:

GCPs_ref <- st_read("H:/Reproject ALS Data test/ALS data/ALS 2019/GCPs/GCPs_ALS_2019.gpkg")
GCPs_2017 <- st_read("H:/Reproject ALS Data test/ALS data/ALS 2017/GCPs/GCPs_ALS_2017.gpkg")

nearest_indices <- st_nearest_feature(GCPs_ref, GCPs_2017)

# Add corresponding matches from sf2 to sf1
GCPs_ref$matchedID <- GCPs_2017$ID[nearest_indices]

# Join attributes
merged_sf <- left_join(as.data.frame(GCPs_ref), as.data.frame(GCPs_2017), by = c("matchedID" = "ID"), suffix = c("_ref", "_test")) %>% 
  mutate(deltaX = X_ref - X_test) %>% 
  mutate(deltaY = Y_ref - Y_test) %>%
  mutate(deltaZ = Z_ref - Z_test) %>%
  filter(abs(deltaX) < 5)

ggplot(data = merged_sf, aes(y = deltaX, x = AOI_ref)) +
  geom_boxplot()

ggplot(data = merged_sf, aes(y = deltaY, x = AOI_ref)) +
  geom_boxplot()

ggplot(data = merged_sf, aes(y = deltaZ, x = AOI_ref)) +
  geom_boxplot()


