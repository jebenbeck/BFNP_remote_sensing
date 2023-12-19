library(sf)
library(tidyverse)
library(mapview)
library(pbapply)



#' list all _sbet.txt files in mission:
file_paths <- list.files(path = "F:/UAV data/Missions/20230912_001 LiDAR LSP Spitzberg/DJI Terra project", 
                       pattern = "*_sbet.txt", recursive = T, full.names = T)

#' assign index to file name:
names(file_paths) <- c(1:length(file_paths))

#' empty list to fill:
flight_paths <- list()

for (i in 1:length(file_paths)) {
  
  #' read in the data:
  data <- read.table(file_paths[[i]], header = F, skip = 2, numerals = "no.loss", sep = "", dec = ".")
  
  #' get the column names:
  header <- read.table(file_paths[[i]], header = T, nrows = 1, sep = "") %>% 
    select(-1) 
  
  #' assign the new column names to the data:
  colnames(data) <- colnames(header)
  
  data_sf <- data %>% 
    #' convert to df:
    as.data.frame() %>% 
    #' convert radians to decimal degrees:
    mutate(`Latitude` = `Latitude`*(180/pi),
           `Longitude` = `Longitude`*(180/pi)) %>% 
    #' Creating sf object with WGS84 (EPSG:4326) CRS
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>% 
    # Transforming XY to EPSG:25832
    st_transform(crs = 25832) %>% 
    #' add the coordinates to the attribute table:
    mutate(X.UTM32 = st_coordinates(.)[, 1],
           Y.UTM32 = st_coordinates(.)[, 2],
           .after = Longitude) %>% 
    #' add flight index to the attribute table:
    mutate(Flight.index = names(file_paths)[[i]],
           .before = Time)
  
  #' add data to list:
  flight_paths[[i]] <- data_sf

}

#' merge to single flight path:
full_mission_path <- bind_rows(flight_paths)

#' Output:
st_write(full_mission_path, "F:/UAV data/Missions/20230912_001 LiDAR LSP Spitzberg/Results/Mission flight path.gpkg", driver = "GPKG", append = F)
