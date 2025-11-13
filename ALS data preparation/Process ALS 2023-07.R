## Info ----------------------------------------------------------------------------------------------------------------


#' Harmonizing the ALS 2023-07 dataset


### Required packages ----


library(lidR)
library(sf)
library(terra)
library(mapview)
library(future)
library(tidyverse)
library(stringr)
library(ggplot2)
library(yardstick)
library(bfnpALSprocessor)
library(ggplot2)



## 1. rename files to suit new tile structure --------------------------------------------------------------------------


#' rename all the files according to the new nomenclature:

# Set target directory

target_dir <- "I:/01_point_clouds/ALS 2023-07/full_extent/00_original"

# List all files in the directory
files <- list.files(target_dir, pattern = "\\.laz$", full.names = TRUE)

# Function to generate new file name
rename_file <- function(full_path) {
  # Extract just the filename
  filename <- basename(full_path)
  
  # Remove the extension suffix
  base <- sub("_classified\\.laz$", "", filename)
  
  # Split at underscore
  parts <- strsplit(base, "_")[[1]]
  
  # Process parts
  part1_new <- paste0(substr(parts[1], 3, 5), "000")
  part2_new <- paste0(parts[2], "000")
  
  # Create new filename
  new_filename <- paste0(part1_new, "_", part2_new, ".laz")
  
  # Return full new path
  file.path(dirname(full_path), new_filename)
}

# Loop through and rename
for (old_path in files) {
  new_path <- rename_file(old_path)
  file.rename(old_path, new_path)
}



## 2. DTM creation -----------------------------------------------------------------------------------------------------


#' read in lascatalog:
ctg <- readALSLAScatalog("I:/01_point_clouds/ALS 2023-07/full_extent/00_original")

#' perform the dtm creation:
ctg_dtm <- catalog_dtm(ctg, output_path = "I:/02_dtms/ALS 2023-07", filename_convention = "{ORIGINALFILENAME}_dtm", 
                       mosaic_result = T, mosaic_name = "ALS_2023-07_DTM_Mosaic", parallel = T, n_cores = 10)



## 3. Normalization ----------------------------------------------------------------------------------------------------


#' read in lascatalog:
ctg <- readALSLAScatalog("I:/01_point_clouds/ALS 2023-07/full_extent/00_original")
st_crs(ctg) <- "EPSG:25832"

#' perform the normalization with "best" dtm dataset:
ctg_normalized <- catalog_normalize(lascatalog = ctg, algorithm = "dtm", dtm_path = "G:/misc/DTM1_combined_17_19_23.tif", 
                                    output_path = "I:/01_point_clouds/ALS 2023-07/full_extent/01_normalized",
                                    parallel = T, n_cores = 6)



## 4. Export footprint polygons ----------------------------------------------------------------------------------------


#' read in lascatalog
ctg <- readALSLAScatalog("I:/01_point_clouds/ALS 2023-07/full_extent/01_normalized")

#' create footprint polygons including tile statistics
ctg_footprints <- catalog_statistics(ctg, parallel = T, n_cores = 18, spatial = T)

#' export polygon file to geopackage:
st_write(ctg_footprints, dsn = "I:/misc/ALS_tiles.gpkg", layer = "ALS 2023-07", append = T)



## 5. Clip to test areas -----------------------------------------------------------------------------------------------


#' read in lascatalog:
ctg <- readALScatalog("I:/01_point_clouds/ALS 2023-07/full_extent/01_normalized")

#' perform clip:
test_areas <- st_read("I:/misc/test_areas.gpkg", layer = "AOIs_UTM")

ctg_test_areas <- bfnpALSprocessor::catalog_clip_polygons(ctg_normalized, input_epsg = "EPSG:25832", 
                                                          output_path = "I:/01_point_clouds/ALS 2023-07/test_areas", filename_convention = "AOI_{name}", 
                                                          polygons = test_areas, parallel = T, n_cores = 8)
