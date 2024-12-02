library(lidR)

## convert asc to point clouds ----


# Step 1: Read the ASC file as a text file
point_data <- read.table("H:/ALS 2012/Punktwolken_asc/spur00001.asc", header = FALSE, sep = "", stringsAsFactors = FALSE)

# Step 2: Rename columns to X, Y, Z (adjust if needed)
colnames(point_data) <- c("X", "Y", "Z", "Pulsewidth", "Intensity", "ReturnNumber", "gpstime")
head(point_data)

# Step 3: Create a LAS object from the data frame
las_data <- LAS(point_data)
st_crs(las_data) <- 31468

# Step 4: Export the LAS object to a LAS file
writeLAS(las_data, "C:/Users/jakob/Desktop/example.las")


## parallel processing

# Load required libraries
library(lidR)
library(pbapply)

# Specify input and output directories
input_dir <- "H:/ALS 2012/Punktwolken_asc/"
output_dir <- "C:/Users/jakob/Desktop/las_output/"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Get a list of all .asc files in the input directory
asc_files <- list.files(input_dir, pattern = "\\.asc$", full.names = TRUE)

# Define a function to process a single ASC file
process_asc_file <- function(asc_file) {
  # Extract the filename without the path and extension
  file_name <- tools::file_path_sans_ext(basename(asc_file))
  
  # Step 1: Read the ASC file as a text file
  point_data <- read.table(asc_file, header = FALSE, sep = "", stringsAsFactors = FALSE)
  
  # Step 2: Rename columns (adjust if needed)
  colnames(point_data) <- c("X", "Y", "Z", "Pulsewidth", "Intensity", "ReturnNumber", "gpstime")
  
  # Step 3: Create a LAS object from the data frame
  las_data <- LAS(point_data)
  st_crs(las_data) <- 31468  # Set the coordinate reference system (adjust as needed)
  
  # Step 4: Export the LAS object to a LAS file
  output_file <- file.path(output_dir, paste0(file_name, ".las"))
  writeLAS(las_data, output_file)
  
  # Return the output file path for logging purposes
  return(output_file)
}

# Use pbapply for parallel processing of the ASC files
output_files <- pblapply(asc_files, process_asc_file)

# Print the output file paths
print(output_files)
