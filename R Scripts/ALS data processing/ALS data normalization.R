library(lidR)


#' read las file
#' it should have classified ground points:
las <- readLAS(system.file("extdata", "Topography.laz", package = "lidR"))

#' normalize point cloud based on DTM:
las_normalized <- lidR::normalize_height(las, algorithm = tin())

#' Step 1: Create a temporary variable to store the original Z values
temp <- las_normalized$Z

# Step 2: Switch Z and Zref
las_normalized$Z <- las_normalized$Zref
las_normalized$Zref <- temp

#' check:
las_normalized$Zref
las_normalized$Z
las_normalized@data
head(las_normalized@data)

#' Visualize:
plot(las_normalized, color = "Z")
plot(las_normalized, color = "Zref")

