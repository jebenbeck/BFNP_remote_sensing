library(sf)
library(tidyverse)
library(mapview)
library(pbapply)



#' list all _sbet.txt files in mission:
file_paths <- list.files(path = "F:/UAV data/Missions/20230912_001 LiDAR LSP Spitzberg/DJI Terra project", 
                       pattern = "*_sbet.txt", recursive = T, full.names = T)



prepare_flight_path <- function(file_path){
  
  #' read in the data:
  data <- read.table(file_path,
                     header = F,
                     skip = 2,
                     numerals = "no.loss",
                     sep = "",
                     dec = ".")
  
  #' get the column names:
  header <- read.table(file_path,
                       header = T,
                       nrows = 1,
                       sep = "") %>% 
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
    mutate(X.UTM32 = st_coordinates(.)[, 1],
           Y.UTM32 = st_coordinates(.)[, 2],
           .after = Longitude)
  
}

test <- pblapply(file_paths, prepare_flight_path)

test_full <- rbind(test)
nrow(test_full)

#' Output:
st_write(data_sf, "F:/UAV data/Missions/20230912_001 LiDAR LSP Spitzberg/Results/Mission flight path.gpkg", driver = "GPKG", append = F)
