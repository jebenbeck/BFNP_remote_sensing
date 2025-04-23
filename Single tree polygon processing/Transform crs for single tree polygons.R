library(sf)
library(tidyverse)
library(pbapply)


#' check reprojection settings for GK->UTM transformations:
sf_proj_network(enable = T)
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
options[1,]$definition #' the transformation in use, must use BETA2007


#' read in all the files that shall be processed:
files <- list.files("E:/Single tree polygons 2017/01_Projected_GK/", pattern = "*.gpkg$", recursive = T, full.names = TRUE)

#' function to transform the data:
transform_polygons <- function(file){
  
  #' get name of the layer:
  filename <- tools::file_path_sans_ext(basename(file))  # Remove .gpkg
  if (nchar(filename) > 45) {
    filename <- paste0(substr(filename, 1, 9), substr(filename, nchar(filename)-1, nchar(filename)))
  } else {
    filename <- substr(filename, 1, 8)
  }
  
  #' read polygons and reproject them: to UTM Zone 32 N:
  polygons_prj <- read_sf(file, quiet = T) %>% st_transform(crs = st_crs("EPSG:25832"))
  
  #' reproject center position of the trees to UTM:
  centers_UTM <- st_drop_geometry(polygons_prj) %>% 
    select(c(ID, X_POS, Y_POS)) %>%                          #' remove unnecessary columns to save memory 
    st_as_sf(coords = c("X_POS", "Y_POS"), crs = 31468) %>%  #' transform to sf to be able to reproject data
    st_transform(test_sf, crs = st_crs("EPSG:25832")) %>%    #' do the transformation
    mutate(                                                  #' add the new coordinates to table as an attribute
      X_POS = st_coordinates(.)[, "X"],
      Y_POS = st_coordinates(.)[, "Y"]) %>% 
    st_drop_geometry()
  
  #' replace the coordinates in the polygon dataset:
  polygons_prj_UTM <- polygons_prj %>%                    
    left_join(centers_UTM, by = "ID", suffix = c("", "_new")) %>%  #' join the new columns with the polygon data
    mutate(X_POS = X_POS_new, Y_POS = Y_POS_new) %>%               #' replace the coordinates
    select(-X_POS_new, -Y_POS_new) %>%                             #' remove duplicate columns
    arrange(ID) %>%                                                #' sort by ID
    mutate(across(c(HEIGHT, TER_HEIGHT, CB_HEIGHT, CROWN_VOL), ~ round(.x, 2)))  #' round tree attribute values to cm
  
  #' export as gpkg:
  st_write(polygons_prj_UTM, dsn = "E:/Single tree polygons 2017/temp/NCUT_polygons_2017_UTM.gpkg", layer = paste0(layername), 
           driver = "GPKG", append = T, quiet = T)

  #' clean data from memory:
  rm(polygons_prj)
  rm(polygons_prj_UTM)
  gc()
  
}  

# Set up parallel backend with 2 cores
cl <- makeCluster(1)
clusterEvalQ(cl, library(sf)) # Load sf on all nodes

#' apply function to all datasets:
pblapply(files, transform_polygons)

stopCluster(cl)