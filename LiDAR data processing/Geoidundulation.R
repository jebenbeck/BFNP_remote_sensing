library(terra)
library(dplyr)
library(lidR)
library(mapview)
library(sf)

### Preprocess the geoid ----

# Read the geoid model (TIFF file)
geoid <- rast("C:/Users/NBW-Ebenbeck_J/Downloads/quasigeoid.geo89.de/quasigeoid/de/windows/geotiff/GCG2016v2023.tif")
plot(geoid)

poly <- as.polygons(geoid, values = F, aggregate = T) 
poly

poly_merged <- terra::aggregate(poly)
mapview(poly_merged)
#' Interpolate for higher resolution and smoother results:

# Create empty target raster
r <- rast(x = ext(geoid), resolution = 100, crs = terra::crs(geoid))

#' interpolate grid:
y <- resample(geoid, r, method = "bilinear")

#' reproject geoid to match point cloud:
geoid_UTM <- project(y, "epsg:25832")


#' export raster:
writeRaster(y, "C:/Users/NBW-Ebenbeck_J/Downloads/quasigeoid.geo89.de/quasigeoid/de/windows/geotiff/GCG2016v2023_100m.tif")

### Perform the geoidundulation on ALS data ----

ALS <- readALSLAS("D:/ALS 2017/NPV_00551.laz")
ALS
crs(ALS) <- "EPSG:25833"

ALS_trans <- st_transform(ALS, "epsg:25832")
ALS_trans@data

ALS_Geoid <- merge_spatial(ALS_trans, y, "Geoidundulation")
ALS_Geoid@data

test <- readALSLAS("D:/ALS 2012/UTM32/spur00001_UTM.laz")

