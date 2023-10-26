require(sf)
require(lidR)
require(tidyverse)
require(mapview)
require(pbapply)


list_files <- list.files("G:/ALS Amtliche Vermessung 2019/Laserpunkte_NP_Bayerischer_Wald_LAZ/Laserpunkte_NP_Bayerischer_Wald_LAZ/368671", pattern = "*.laz", full.names = T)


get_ALS_footprint_polygons <- function(i) {
  file_name <- basename(i)
  
  pointcloud <- lidR::readLAS(i) 
  
  pointcloud_density <- density(pointcloud)
  pointcloud_npoints <- npoints(pointcloud)
  
  polygon_bbox <- pointcloud %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_set_crs(25832) %>% 
    st_as_sf() %>% 
    mutate(tile_name = file_name, 
           point_density = pointcloud_density,
           n_points = pointcloud_npoints)
  
  return(polygon_bbox)
  
  rm(pointcloud)
}


list_polygons <- pblapply(list_files, get_ALS_footprint_polygons)

polygons <- bind_rows(list_polygons)

mapview(polygons)

st_write(polygons, "G:/ALS Amtliche Vermessung 2019/Laserpunkte_NP_Bayerischer_Wald_LAZ/Abdeckung.gpkg", driver = "GPKG")
