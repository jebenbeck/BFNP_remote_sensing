library(RSDB)
library(sf)
library(raster)
library(terra)


# ---- Connect to RSDB server ---- #

#' Provide the login-credentials in an local R file on your computer or via an object (format: "username:password")
source("C:/Users/Rieser/OneDrive/BFNP/Projects/Forest Ecosystem Monitoring/R Scripts/RSDB/RSDB credentials.R")

#' Connect to the server
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 



## 1. Query raster data ------------------------------------------------------------------------------------------------


# ---- Get raster data ---- #

#' Select raster database:
ALS_metrics.db <- db$rasterdb("LiDAR_metrics_2017_10m")

#' define AOI (can be any spatial object)
#' in this case, it is the full extent of the data
AOI <- ALS_metrics_2017$extent

#' query rasterstack of ALS Metrics: 
ALS_metrics.raster <- ALS_metrics.db$raster(ext = AOI)
plot(ALS_metrics.raster)


# ---- Postprocessing ---- #

#' reproject to coordinate system (if necessary):
ALS_metrics_prj.raster <- projectRaster(from = ALS_metrics.raster, crs = "EPSG:25833")

#' convert to terra raster object (to preserve the layer names):
ALS_metrics_prj.rast <- rast(ALS_metrics_prj.raster)

#' export raster stack to disk:
terra::writeRaster(ALS_metrics_prj.rast, "F:/ALS_metrics_2017.tif", overwrite = T)



## 2. Query vector data ------------------------------------------------------------------------------------------------


#' Select vector database:
Areas_NPBW.db <- db$vectordb("Test_single_tree_polygons")

#' Get the polygons:
Areas_NPBW.poly <- Areas_NPBW.db$getVectors()

#' look at data:
Areas_NPBW.poly
plot(Areas_NPBW.poly)