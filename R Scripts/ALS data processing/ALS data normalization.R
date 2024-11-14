library(lidR)


#' read las file
#' it should have classified ground points:
las <- readLAS("H:/Daten ADA/ALS_20230715_Scheuereck.las")
las
#' normalize point cloud based on DTM:
las_normalized <- lidR::normalize_height(las, algorithm = tin())

#' Step 1: Create a temporary variable to store the original Z values
temp <- las_normalized$Z

# Step 2: Switch Z and Zref
las_normalized$Z <- las_normalized$Zref

#' add the normalized Z values as a proper attribute for export:
las_output <- add_lasattribute(las_normalized, x = temp, name = "Z_norm", desc = "Normalize Height")

#' add correct coordinate system:
st_crs(las_output) <- 25832

#' Visualize:
plot(las_normalized, color = "Z")
plot(las_normalized, color = "Zref")

#' export:
writeLAS(las_output, "H:/Daten ADA/ALS_20230715_Scheuereck_normalized.las")

#' check:
las_2 <- readLAS("H:/Daten ADA/ALS_20230715_Scheuereck_normalized.las")
las_2@data
las_2
