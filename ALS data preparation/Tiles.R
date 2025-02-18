## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2025-01-15
#' Status: Work in progress 


### Purpose of script ----

#' Creates a grid of 1km² across the full Bohemian Forest ecosystem. This grid is used to retile las files of ALS data 
#' for more intuitive data storage and better overview.


### Required packages ----

library(sf)
library(tidyverse)
library(mapview)



## Make Grid -----------------------------------------------------------------------------------------------------------


#' Define a known starting point for the bottom-left corner (detected manually using GIS)
bottom_left_coords <- c(791118, 5371732)

#' Ensure the coordinates are multiples of 1000 for 1 km grid:
bottom_left_coords <- floor(bottom_left_coords / 1000) * 1000

#' Create a bounding box (polygon) for the area that should be covered by the grid (100x100 km)
x_min <- bottom_left_coords[1]
y_min <- bottom_left_coords[2]
x_max <- x_min + 100000  # 100 km in the X direction
y_max <- y_min + 100000  # 100 km in the Y direction

#' Create a bounding box as a polygon
bbox <- st_sfc(st_polygon(list(matrix(c(x_min, y_min, 
                                        x_max, y_min, 
                                        x_max, y_max, 
                                        x_min, y_max, 
                                        x_min, y_min), 
                                      ncol = 2, byrow = TRUE))),
               crs = 25832)

#' Create the grid using the bounding box (polygon) and cell size of 1000 meters:
grid <- st_as_sf(st_make_grid(bbox, cellsize = 1000, what = "polygons"))

#' Extract coordinates of all tiles:
bbox_list <- lapply(st_geometry(grid), st_bbox)

#' Convert the coordinates to df:
maxmin <- as.data.frame(matrix(unlist(bbox_list), nrow=nrow(grid), byrow = T ))
names(maxmin) <- names(bbox_list[[1]])

#' Construct name for each tile:
maxmin$tile_name <- paste0(as.character(format(maxmin$xmin), scientific = F), 
                           "_", 
                           as.character(format(maxmin$ymin), scientific = F))

#' Add name to spatial grid:
grid$Tile.name <- maxmin$tile_name

#' check:
mapview(grid)

#' Export grid to disc:
st_write(grid, "C:/Users/NBW-Ebenbeck_J/Downloads/ALS_tiles.gpkg", layer = "Overview", append = F)
