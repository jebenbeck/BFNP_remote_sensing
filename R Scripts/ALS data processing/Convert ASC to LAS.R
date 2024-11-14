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
x <- readLAS("C:/Users/jakob/Desktop/example.las")
