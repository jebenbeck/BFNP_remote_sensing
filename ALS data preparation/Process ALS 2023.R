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
