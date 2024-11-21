library(lidR)


#' read las file
#' it should have classified ground points:
las <- readLAS("F:/Testdaten Befliegungen 2023/2017/ALS_2017_Sulzschachten.las")

#' normalize point cloud based on DTM:
las_normalized <- lidR::normalize_height(las, algorithm = tin())

#' Step 1: Create a temporary variable to store the original Z values
temp <- las_normalized$Z

# Step 2: Switch Z and Zref
las_normalized$Z <- las_normalized$Zref

#' add the normalized Z values as a proper attribute for export:
las_output <- add_lasattribute(las_normalized, x = temp, name = "NormalizedHeight", desc = "Height above ground")
las_output@data
#' add correct coordinate system:
st_crs(las_output) <- 25832

#' Visualize:
plot(las_output, color = "Z")
plot(las_output, color = "NormalizedHeight")

#' export:
writeLAS(las_output, "F:/Testdaten Befliegungen 2023/2017/ALS_2017_Sulzschachten_normalized.las")

rm(las, las_normalized, las_output)
gc()
#' check:
las_2 <- readLAS("H:/Testdaten Befliegungen 2023/Output/ALS_20230715_Sulzschachten.las")
