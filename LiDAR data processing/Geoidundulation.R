library(terra)
library(dplyr)
library(lidR)
library(mapview)
library(sf)

# Read the geoid model (TIFF file)
geoid <- rast("C:/Users/NBW-Ebenbeck_J/Downloads/quasigeoid.geo89.de/quasigeoid.geo89.de/quasigeoid/de/windows/geotiff/GCG2016v2023.tif")
plot(geoid)

#' reproject geoid to match point cloud:
geoid_UTM <- project(geoid, "epsg:25832")

# Define the extent (in UTM zone 32N coordinates)
ext <- ext(450000, 550000, 5500000, 5600000)

# Define the cell size (resolution) in meters
cell_size <- c(30, 30)  # 100 meters by 100 meters

# Define the CRS as EPSG:25832 (UTM zone 32N)
crs_epsg <- "EPSG:25832"  # UTM zone 32N

# Create the empty raster
r <- rast(ext, resolution = cell_size, crs = crs_epsg)

#' interpolate grid:
y <- resample(geoid_UTM, r, method = "bilinear")
mapview(y)

#' export raster:
writeRaster(y, "C:/Users/NBW-Ebenbeck_J/Downloads/quasigeoid.geo89.de/quasigeoid.geo89.de/quasigeoid/de/windows/geotiff/GCG2016v2023_30m.tif")

ALS <- readALSLAS("D:/ALS 2017/NPV_02885.laz")
crs(ALS) <- "EPSG:25833"

ALS_trans = st_transform(ALS, "epsg:25832")

ALS_Geoid <- merge_spatial(ALS_trans, y, "Geoid")
ALS_Geoid@data$Geoid
