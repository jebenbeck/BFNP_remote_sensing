library(sf)
library(tidyverse)
library(mapview)

#' Accurracy assessment 

#' load AOIs:
AOIs <- st_read("H:/Reproject ALS Data test/AOIs.gpkg", layer = "AOIs_UTM")
AOIs$name

#' preprocessing the GCPs derived from ALS point clouds in cloudcompare:

file_list <- list.files("H:/Reproject ALS Data test/ALS data/ALS 2019/GCPs", pattern = "\\.txt$", full.names = T)

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
st_write(data, "H:/Reproject ALS Data test/ALS data/ALS 2019/GCPs/GCPs_ALS_2019.gpkg")

