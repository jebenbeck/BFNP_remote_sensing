## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2025
#' Status: Work in progress 


### Purpose of script ----


### Notes ----


### Required datasets ----


### Required packages ----

library(lidR)
library(sf)
library(mapview)
library(future)
library(tidyverse)
library(pbapply)
library(stringr)
library(bfnpALSprocessor)
library(RCSF)


## 1. Convert ASCII files to LAS ---------------------------------------------------------------------------------------


#' Takes all LiDAR Point Clouds stored in ASC format in folder and converts them to LAZ
#' Works specifically with the data acquired by ALS in 2012 in the BFNP.


#' Specify input and output paths:
#input_dir <- paste0(path_drive, "ALS 2012/Punktwolken_asc/")
input_dir <- "C:/Users/NBW-Ebenbeck_J/Desktop"
#output_dir <- paste0(path_drive, "ALS 2012/Punktwolken_laz/")
output_dir <- "C:/Users/NBW-Ebenbeck_J/Desktop"


#' number of files to process (only for testing)
n_files <- 0

#' Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

#' Get a list of all .asc files in the input directory
asc_files <- list.files(input_dir, pattern = "\\.asc$", full.names = TRUE)

#' subset of list:
if (n_files > 0) {
  asc_files <- asc_files[1:n_files]
}

#' Get a list of all already processed files in the output directory
processed_files <- list.files(output_dir, pattern = "\\.laz$", full.names = FALSE)
processed_files <- tools::file_path_sans_ext(processed_files) # Remove extensions for comparison

#' Filter asc_files to skip already processed ones
asc_files_to_process <- asc_files[!tools::file_path_sans_ext(basename(asc_files)) %in% processed_files]
asc_files_to_process

#' Define a function to process a single ASC file
process_asc_file <- function(asc_file) {
  
  #' Extract the filename without the path and extension
  file_name <- tools::file_path_sans_ext(basename(asc_file))
  
  #' Step 1: Read the ASC file as a text file
  point_data <- read.table(asc_file, header = FALSE, sep = " ", stringsAsFactors = FALSE)
  
  #' Step 2: Rename columns (adjust if needed)
  colnames(point_data) <- c("X", "Y", "Z", "Pulsewidth", "Intensity", "ReturnNumber", "gpstime")
  
  #' Step 3: Create a LAS object from the data frame
  las_data <- LAS(point_data) %>% 
    #' add Pulsewidth as attribute to the las file:
    add_lasattribute(., point_data$Pulsewidth, name = "Pulsewidth", desc = "Pulsewidth")
  
  #' Set the coordinate reference system (adjust as needed)
  st_crs(las_data) <- 31468  
  
  #' Step 4: Export the LAS object to a LAS file
  output_file <- file.path(output_dir, paste0(file_name, ".laz"))
  writeLAS(las_data, output_file)
  
  #' Return the output file path for logging purposes
  return(output_file)
}

#' Use pbapply for processing all ASC files in parallel:
cluster <- parallel::makeCluster(4)

parallel::clusterExport(cluster, varlist = c("readLAS", "writeLAS", "basename", "read.table", "output_dir", "LAS", 
                                             "add_lasattribute", "colnames", "st_crs"))
parallel::clusterEvalQ(cluster, library(lidR)) # Load lidR on all nodes
parallel::clusterEvalQ(cluster, library(tidyverse)) # Load tidyverse on all nodes

#output_files <- pblapply(asc_files_to_process, process_asc_file, cl = cluster)
output_files <- pblapply(asc_files_to_process, process_asc_file)

parallel::stopCluster(cluster)



## 2. Add number of returns to point clouds ----------------------------------------------------------------------------


ctg <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_laz")
plot(ctg, mapview = T)

#' Function to calculate the number of returns and add it to the las data
catalog_add_nReturns <- function(lascatalog, output_path, parallel = T, n_cores = 2) {
  
  #' apply options to lascatalog
  opt_output_files(lascatalog) <- paste0(output_path, "/{ORIGINALFILENAME}")
  opt_laz_compression(lascatalog) <- TRUE
  opt_independent_files(lascatalog) <- TRUE
  
  add_nReturns <- function(las) {
    
    #' calculate number of returns:
    nReturns <- las@data %>% 
      select(ReturnNumber) %>% 
      mutate(pulse_id = cumsum(ReturnNumber == 1)) %>%  #' create a unique pulse ID
      group_by(pulse_id) %>%                            #' group by pulse ID 
      mutate(NumberOfReturns = max(ReturnNumber)) %>%   #' assign max return number per pulse
      ungroup() %>%
      select(-pulse_id, -ReturnNumber) %>% 
      unlist()
    
    #' add data to las file as argument:
    las$NumberOfReturns <- nReturns
    
    return(las)
  }
  
  #' plan parallel processing
  if (parallel == TRUE) {
    plan(multisession, workers = n_cores)
    message(paste("Parallel processing will be used with", n_cores, "cores"))
  } else {
    warning("No parallel processing in use", call. = F, immediate. = T)
  }
  
  #' apply function to lascatalog:
  new_catalog <- catalog_map(lascatalog, add_nReturns)
  return(new_catalog)
  
}

#' apply function to catalog:
ctg_nReturns <- catalog_add_nReturns(ctg, output_path = "G:/ALS 2012-06/pointclouds_nReturns", parallel = T, n_cores = 14)


### Export polygons ----

ctg_nReturns <- readALScatalog("G:/ALS 2012-06/pointclouds_nReturns")

ctg_polygons <- bfnpALSprocessor::catalog_to_polygons(ctg_nReturns)
st_write(ctg_polygons, "G:/ALS 2012-06/Tiles.gpkg")


### Clip to testdata ----

ctg_nReturns <- readALScatalog("G:/ALS 2012-06/pointclouds_full_nReturns")

test_areas <- st_read("G:/misc/test_areas.gpkg", layer = "AOIs_GK")

ctg_test_areas <- bfnpALSprocessor::catalog_clip_polygons(ctg_nReturns, input_epsg = "EPSG:31468", 
        output_path =  "G:/ALS 2012-06/pointclouds_test_areas", filename_convention = "AOI_{name}", polygons = test_areas, parallel = T, n_cores = 4)


## 3. Reproject to UTM32 -----------------------------------------------------------------------------------------------


ctg <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_full_nReturns")

ctg_reprojected <- catalog_reproject(ctg, input_epsg = "EPSG:31468", output_epsg = "EPSG:25832", 
                                     output_path = "G:/ALS 2012-06/pointclouds_full_UTM32", parallel = T, n_cores = 10)


## 4. Retile to UTM grid -----------------------------------------------------------------------------------------------

ctg <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_full_UTM32")
ctg_retiled <- bfnpALSprocessor::catalog_retile_template(ctg, output_path = "G:/ALS 2012-06/pointclouds_full_retiled")



## 5. Classify ---------------------------------------------------------------------------------------------------------

ctg <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_full_retiled")
plot(ctg, mapview = T)

ctg_classified <- catalog_classify_ground(ctg, output_path = "G:/ALS 2012-06/pointclouds_full_classified", parallel = T, n_cores = 6)
plot(ctg_classified, mapview = T)

#' export statistics:
ctg_classified <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_full_classified")
las_check(ctg_classified)
plot(ctg_classified, mapview = T)

ctg_statistics <- catalog_statistics(ctg_classified, parallel = T, n_cores = 18, spatial = T)
ctg_statistics

#' export polygon file to geopackage:
st_write(ctg_statistics, dsn = "G:/misc/ALS_tiles.gpkg", layer = "ALS_2012-06", append = T)


## 6. Normalize --------------------------------------------------------------------------------------------------------


ctg <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_full_classified")

ctg_normalized <- catalog_normalize(lascatalog = ctg, algorithm = "dtm", dtm_path = "G:/misc/DTM1_combined_17_19_23.tif", output_path = "G:/ALS 2012-06/pointclouds_full_normalized",
                                    parallel = T, n_cores = 6)


## 7. create lax -------------------------------------------------------------------------------------------------------

ctg_retiled <- readALSLAScatalog("G:/ALS 2012-06/pointclouds_full_classified")
lidR:::catalog_laxindex(ctg_retiled)

