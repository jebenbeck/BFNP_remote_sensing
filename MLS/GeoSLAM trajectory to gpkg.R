#' Converts a trajectory of GeoSLAM Scanner to geopackage
#' Works with the georeferenced trajectory file generated from GeoSLAM Connect

library(sf)

#' Function:
txt_to_gpkg <- function(input_txt, output_gpkg, geometry_column, crs = 25832) {
  
  #' read txt to df: 
  data <- read.table(input_txt, header = TRUE, sep = " ", dec = ".")
  
  #' make sf-object with geometry column: 
  sf_data <- st_as_sf(data, coords = c("x", "y"), crs = crs)
  
  #' confert sf object to geopackage and write to disk: 
  st_write(sf_data, output_gpkg, layer = "traj", driver = "GPKG")
  
}

#' run function example:
txt_to_gpkg("F:/Datenaufnahme Referenzflächen 2023/HTO_21_Lärchenberg1/Geoslam_Projekt_HTO_21_Laerchenberg1/Input/results_traj.txt", 
            "F:/Datenaufnahme Referenzflächen 2023/HTO_21_Lärchenberg1/trajectory/traj_21.gpkg",
            c("´x", "y"))
