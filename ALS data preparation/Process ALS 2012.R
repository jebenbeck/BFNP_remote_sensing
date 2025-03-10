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

### Required functions and scripts ----

source("ALS data preparation/Processing functions master.R")

### Set working directories ----

#' set wd of drive where the ALS 2017 database is stored. Must be changed when switching PCs

path_drive <- "D:/"



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



## 2. Add number of returns argument ---------------------------------------------------------------------------


test <- readALSLAS("C:/Users/NBW-Ebenbeck_J/Desktop/spur00497.laz")

calculate_nReturns <- function(las_file) {
  las_data <- las_file@data %>% 
    select(ReturnNumber) %>% 
    mutate(pulse_id = cumsum(ReturnNumber == 1)) %>%  #' create a unique pulse ID
    group_by(pulse_id) %>%                            #' group by pulse ID 
    mutate(NumberOfReturns = max(ReturnNumber)) %>%   #' assign max return number per pulse
    ungroup() %>%
    select(-pulse_id, -ReturnNumber) %>% 
    unlist()
  
  #' add data to las file as argument:
  las_file$NumberOfReturns <- las_data
  
  return(las_file)
}

test_2 <- calculate_nReturns(test)

head(test@data)


## 3. Reproject to UTM32 -----------------------------------------------------------------------------------------------
