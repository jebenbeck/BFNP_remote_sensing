library(lidR)
library(pbapply)
library(parallel)

#' Define directories
input_directory <- "F:"  
output_directory <- "F:/clipped"  

# Create output directory if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

#' Find all "_rigid.laz" files recursively
rigid_laz_files <- list.files(
  path = input_directory,
  pattern = "_rigid\\.laz$", # Match "_rigid.laz"
  full.names = TRUE,          # Return full file paths
  recursive = TRUE            # Search subdirectories
)

#' Function to clip and save LAS files
process_file <- function(file_path) {
  #' Load the LAS file
  las <- readLAS(file_path)
  
  #' Perform the circular clipping (center at (0,0), radius 14)
  clipped_las <- clip_circle(las, 0, 0, 14)
  
  #' Generate output file name
  output_file <- gsub("_rigid\\.laz$", "_clipped.las", basename(file_path))
  output_path <- file.path(output_directory, output_file)
  
  # Save the clipped LAS file
  writeLAS(clipped_las, output_path)
  return(output_path)
}


# Set up parallel backend with 2 cores
n_cores <- 2
cl <- makeCluster(n_cores)  # Create a cluster with 2 cores
clusterExport(cl, varlist = c("clip_circle", "readLAS", "writeLAS", "gsub", "is.empty", "basename", "file.path", "output_directory"))
clusterEvalQ(cl, library(lidR)) # Load lidR on all nodes

# Apply the function in parallel with progress tracking
output_files <- pblapply(rigid_laz_files, process_file, cl = cl)

# Stop the cluster
stopCluster(cl)


gc()
