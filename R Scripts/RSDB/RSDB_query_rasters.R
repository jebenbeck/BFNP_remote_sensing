#' load the required packages:
library(RSDB)
library(sf)
library(raster)
library(terra)


##' Connect to the RSDB Server

#' Provide the login-credentials in an local R file on your computer or via an object:

#' format: "username:password"
#' example: 
credentials <- "jakob.rieser:examplepassword"

#' Connect to the server
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 


##' Get the Raster Data:

#' Select raster database:
ALS_metrics_2017 <- db$rasterdb("LiDAR_metrics_2017_10m")

#' define AOI (can be any spatial object):
AOI <- ALS_metrics_2017$extent

#' query rasterstack of ALS Metrics: 
raster_test <- ALS_metrics_2017$raster(ext = AOI)
plot(raster_test)

#' reproject to coordinate system (if necessary):
raster_test_prj <- projectRaster(from = raster_test, crs = "EPSG:25833")

#' convert to terra raster object (to preserve the layer names):
raster_test_prj <- rast(raster_test_prj)

#' export raster stack to disk:
terra::writeRaster(raster_test_prj, "F:/ALS_metrics_2017.tif", overwrite = T)
