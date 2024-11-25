## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 25.11.2024
#' Status: Work in progress 


### Purpose of script ----

#' Takes all LiDAR Point Clouds stored in ASC format in folder and converts them to LAZ
#' Works specifically with the data acquired by ALS in 2012 in the BFNP.

### Notes ----

#' Currently, there is one column where it is unclear what it is. It cannot be determined based on the documentation
#' For now this line is skipped in the output

### Required datasets ----

#' Point clouds in ASC format

### Required packages ----

library(lidR)
library(pbapply)
library(tidyverse)


## Skript --------------------------------------------------------------------------------------------------------------


#' Specify input and output directories
input_dir <- "E:/ALS 2012/Punktwolken_asc/"
output_dir <- "E:/ALS 2012/Punktwolken_laz/"

#' number of files to process (only for testing)
n_files <- 2

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

#' Define a function to process a single ASC file
process_asc_file <- function(asc_file) {
  #' Extract the filename without the path and extension
  file_name <- tools::file_path_sans_ext(basename(asc_file))
  
  #' Step 1: Read the ASC file as a text file
  point_data <- read.table(asc_file, header = FALSE, sep = "", stringsAsFactors = FALSE)
  
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
output_files <- pblapply(asc_files, process_asc_file, cl = 2)

#' Test:
test <- readLAS("E:/ALS 2012/Punktwolken_laz/spur00001.laz")
test@data