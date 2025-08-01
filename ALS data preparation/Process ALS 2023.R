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
library(terra)
library(mapview)
library(future)
library(tidyverse)
library(stringr)
library(ggplot2)
library(yardstick)
library(bfnpALSprocessor)

### Required functions and scripts ----

source("ALS data preparation/Processing functions master.R")


## 1. rename files -------------------------------------------------------------


#' rename all the files according to the new nomenclature:

# Load required library
library(stringr)

# Set target directory

target_dir <- "G:/ALS 2023-07/pointclouds_classified"

# List all files in the directory
files <- list.files(target_dir, pattern = "\\.laz$", full.names = TRUE)
files

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


## 1. Classify ground points ---------------------------------------------------


## 3. DTM creation -------------------------------------------------------------




#' read in lascatalog:
ctg <- readALSLAScatalog("G:/ALS 2023-07/pointclouds_classified")
las_check(ctg)
plot(ctg, mapview = T)

#' perform the dtm creation:
ctg_dtm <- catalog_dtm(ctg, output_path = "G:/ALS 2023-07/dtm", filename_convention = "{ORIGINALFILENAME}_dtm", mosaic_result = T,
                       mosaic_name = "ALS_2023-07_DTM_Mosaic", parallel = T, n_cores = 10)





## 4. Normalization ----------------------------------------------------------------------------------------------------------

#' read in lascatalog:

ctg <- readALSLAScatalog("G:/ALS 2023-07/pointclouds_classified")
st_crs(ctg) <- "EPSG:25832"
plot(ctg, mapview = T)

#' read in roi to where the dtm of the bavarian surveying administration is valid:
polygon <- st_read("G:/misc/DTM1_NPV_coverage/NPV_polygon.shp")

#' read in dtm:
dtm <- rast("E:/UAV_data/other_data/DTM1_Bayern_NPV.tif")

#' clip lascatalog to polygon: 
ctg_subset <- catalog_intersect(ctg, polygon)
plot(ctg_subset)

#' perform the normalization with dtm:
catalog_normalize(lascatalog = ctg_subset, dtm = dtm, output_path = "G:/ALS 2023-07/pointclouds_normalized_new", 
                  filename_convention = "{ORIGINALFILENAME}", parallel = T, n_cores = 8)

las_check(ctg)
plot(ctg)


