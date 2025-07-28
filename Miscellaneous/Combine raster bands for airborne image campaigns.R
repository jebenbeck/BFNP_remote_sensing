## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 03.06.2025
#' Status: Working 


### Purpose of script ----

#' In the Bavarian forest national park there are annual campaigns to generate aerial imagery of the full area by airplane. 
#' Since 2004, data is collected in 4 bands: R, G, B, I. The original data and all processed image results are stored in 
#' 2 different files with two different band combinations: color-infrared (I, R, G) and true-color (R, G, B). As this is 
#' not necessary and takes up a lot of disk space, the data can simply be stored in single images with four bands: R, G, B, I.

#' This script is exactly doing that: it takes the original files (stored in seperate folders) and merges them into a 
#' single file. The script is able to perform this in parallel to make it more efficient. It is basically a single function 
#' that can be called from anywhere and just needs input data. It provides a progress bar informing about the progress of
#' the processing. 

#' Please make sure there is enough disk space available at the output location. The results will take up 66% of the 
#' original data (RGB and CIR combined)


### Required datasets ----

#' 1. a folder with RGB datasets
#' 2. a folder with CIR datasets (names must match RGB data)
#' 3. a folder where the data is exported to


### Required packages ----

library(terra)
library(pbapply)
library(parallel)



## Image processing ----------------------------------------------------------------------------------------------------


#' function to process images:

combine_images <- function(rgb_dir, cir_dir, output_dir, n_cores) {

  #' list all RGB files
  rgb_files <- list.files(rgb_dir, pattern = "*\\.tif$", full.names = TRUE)
  
  process_file <- function(rgb_path) {
  
    #' extract the common suffix part of the filename
    filename <- basename(rgb_path)
    suffix <- sub("^RGB_", "", filename)
    
    #' construct corresponding CIR file path
    cir_path <- file.path(cir_dir, paste0("CIR_", suffix))
    
    #' check if CIR file exists
    if (!file.exists(cir_path)) {
      warning(paste("CIR file not found for:", filename))
      next
    }
    
    #' read RGB and CIR images
    rgb_image <- rast(rgb_path)
    i_image <- rast(cir_path, lyrs = 1)  # Only use first layer from CIR
    
    #' combine RGB and IR channels
    rgbi_image <- c(rgb_image, i_image)
    names(rgbi_image) <- c("R", "G", "B", "I")
    
    #' construct output path
    output_path <- file.path(output_dir, paste0("RGBI_", suffix))
    
    #' write to disk
    terra::writeRaster(rgbi_image, output_path, overwrite = TRUE, gdal = c("PHOTOMETRIC=MINISBLACK", "COMPRESS=LZW"))
    
    return(output_path)
  }
  
  #' set up parallel processing:
  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterEvalQ(cluster, library(terra)) # Load terra on all nodes
  
  #' process the images:
  results <- pblapply(rgb_files, process_file, cl = cluster)
  
  parallel::stopCluster(cluster)
  
  invisible(results)
}

#' apply function:
combine_images(rgb_dir = "C:/Users/NBW-Ebenbeck_J/Desktop/RGB",       #' defines location of the RGB data  
               cir_dir = "C:/Users/NBW-Ebenbeck_J/Desktop/CIR",       #' defines location of the CIR data
               output_dir = "C:/Users/NBW-Ebenbeck_J/Desktop/RGBI",   #' defines output location
               n_cores = 2)                                           #' number of cores used for parallel processing
