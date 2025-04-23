library(sf)
library(tidyverse)
library(parallel)
library(doParallel)
library(foreach)
library(pbapply)


#' check reprojection settings for GK->UTM transformations:
sf_proj_network(enable = T)
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
options[1,]$definition #' the transformation in use, must use BETA2007


#' read in all the files that shall be processed:
files <- list.files("E:/Single tree polygons 2017/01_Projected_GK/", pattern = "*.gpkg$", recursive = T, full.names = TRUE)
filenames <- basename(files)
layernames <- str_sub(filenames, start = 1, end = 8)

#' Set up the parallel computation: 
no_cores <- 8   #' number of cores
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)


foreach(i = 1:length(files), .packages = "sf") %dopar% {
  
  #' read polygons and reproject them: to UTM Zone 32 N:
  polygons_prj <- read_sf(files[[4]], quiet = T) %>% st_transform(crs = st_crs("EPSG:25832"))
  
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
  # st_write(polygons_prj, dsn = paste0("D:/Single tree polygons 2017/Projected_UTM/", filenames[[i]]), driver = "GPKG", append = F)
  st_write(polygons_prj_UTM, dsn = "E:/Single tree polygons 2017/temp/NCUT_polygons_2017_beta.gpkg", layer = paste0(layernames[[4]]), driver = "GPKG", append = T)
  
  #' clean data from memory:
  rm(polygons_prj)
  rm(polygons_prj_UTM)
  gc()
}

stopCluster(cl)

sf_proj_network(enable = T)
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
View(options)
