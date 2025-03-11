library(sf)
library(terra)
library(tidyverse)
library(pbapply)



### for small datasets ----


#' load in list of rasters:
raster_files <- list.files("F:/DGM1_Bayern", pattern = "*.tif$", recursive = T, full.names = TRUE)

#' load in rasters:
raster_list <- lapply(raster_files, rast)

#' convert to a spatial raster collection:
raster_sprc <- sprc(raster_list)

#' make mosaic from raster collection:
terra::merge(raster_sprc, filename = paste0("F:/DGM1_Bayern/", "DGM1_Bayern_gesamt.tif"), overwrite = T)



### for large raster datasets ----


#' load in list of rasters:
raster_files <- list.files("F:/DGM1_Bayern", pattern = "*.tif$", recursive = T, full.names = TRUE)
raster_files

tiles <- st_read("L:/LIDAR/ALS_tiles.gpkg", layer = "Overview")

st_layers("G:/Projektdaten/GIP Ebenbeck/Daten für Inventurdatenbank/Nationalpark Basisdaten.gpkg")
AOI <- st_read("G:/Projektdaten/GIP Ebenbeck/Daten für Inventurdatenbank/Nationalpark Basisdaten.gpkg", layer = "Aussengrenze aktuell") %>% 
  st_buffer(1000)

# Determine if each polygon is completely within the overlay polygon
tiles$covered <- st_intersects(tiles, AOI, sparse = FALSE)[,1]

#' reformat file name:
tiles_new <- tiles %>% separate(Tile.name, into = c("Tile_x", "Tile_y"), sep = "_") %>% 
  mutate(Tile_x = str_sub(Tile_x, start = 1, end = 3),
         Tile_y = str_sub(Tile_y, start = 1, end = 4),
         Tile.name.new = paste0(Tile_x, "_", Tile_y, ".tif")) 

#' filter the files to keep only the ones overlapping: 
filtered_files <- raster_files[basename(raster_files) %in% tiles_new$Tile.name.new[tiles_new$covered]]
filtered_files_2 <- filtered_files[1:3]
filtered_files_2

#' load in rasters:
raster_list <- lapply(filtered_files, rast)

#' convert to a spatial raster collection:
raster_sprc <- sprc(raster_list)

# Function to modify raster values: remove last two of four decimal digits
modify_raster <- function(r) {
  r <- round(r, 2)  # Keep only 2 decimal places
  return(r)
}

# Apply the function to each raster in the collection
modified_rasters <- pblapply(raster_sprc, modify_raster)

# Convert back to a SpatRasterCollection
modified_collection <- sprc(modified_rasters)

#' make mosaic from raster collection:
terra::merge(modified_collection, filename = "F:/DGM1_Bayern_NPV.tif", overwrite = T, wopt= list(datatype = "FLT4S", filetype = "GTiff", todisk = TRUE, gdal=c("COMPRESS=LZW", "TILED=YES")))