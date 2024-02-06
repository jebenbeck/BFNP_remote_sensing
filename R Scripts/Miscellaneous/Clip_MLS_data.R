library(lidR)
library(sf)


#' define files
path_to_las <- "F:/MLS Data/Aufnahme Referenzflächen 2023/Datensicherung 20231218/HTO_59_Recherau/Geoslam_Projekt_HTO_59_Recherau/Input/2023-08-22_11-04-31_HTO_59_Recherau_non-rigid.laz"
path_to_polygon <- "F:/MLS Data/Aufnahme Referenzflächen 2023/Datensicherung 20231218/HTO_59_Recherau/Trajektorie/polygon_traj_59.shp"
path_to_output <- "F:/MLS Data/Aufnahme Referenzflächen 2023/Datensicherung 20231218/HTO_59_Recherau/Ergebnisse/"
name_of_output <- "Recherau_clipped"


### The script ----


#' import polygon:
input_polygon <- st_read(path_to_polygon)

#' import las file that should be clipped:
input_las <- readLAS(path_to_las)

#' change the coordinate system of the input_las file to EPSG:25832
st_crs(input_las) <- 25832

#' clip las-file to polygon and export to disk:
clipped_las <- clip_roi(input_las, input_polygon)

#' export lasfile to disk:
writeLAS(clipped_las, paste0(path_to_output, name_of_output, ".laz")

#' clear RAM:
rm(input_las, clipped_las)
gc()
