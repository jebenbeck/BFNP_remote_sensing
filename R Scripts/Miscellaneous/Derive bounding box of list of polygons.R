library(RSDB)
library(sf)
library(dplyr)

file_list <- list.files(path = "C:/Users/Rieser/Desktop/Polygone/", pattern = "*.shp$", full.names = T)
polygon_list <- lapply(file_list, read_sf)

polygon_list_sub <- lapply(polygon_list, function(x) {
  st_zm(sf::st_geometry(x), drop = T)
})


all_polygons <- do.call(st_union, polygon_list_sub)
plot(all_polygons)

all_polygons_prj <- st_transform(all_polygons, crs = "EPSG:32632")

bbox <- st_bbox(all_polygons_prj)

mapview::mapview(bbox)
st_write(bbox, "C:/Users/Rieser/Desktop/Polygone/Bbox.shp")
