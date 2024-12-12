library(RSDB)
library(sf)
library(raster)
library(terra)
library(mapview)
library(tidyverse)
library(lidR)



# ---- Connect to RSDB server ---- #

#' Provide the login-credentials in an local R file on your computer or via an object (format: "username:password")
source("C:/Users/Rieser/OneDrive/BFNP/Documents/RSDB/RSDB credentials.R")


#' Connect to the server
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 


## 1. Query raster data ------------------------------------------------------------------------------------------------


# ---- Get raster data ---- #

#' Select raster database:
ALS_metrics.db <- db$rasterdb("LiDAR_metrics_ALS_2017-06_10m")

#' define AOI (can be any spatial object)
#' in this case, it is the full extent of the data
AOI <- ALS_metrics.db$extent

#' query rasterstack of ALS Metrics: 
ALS_metrics.raster <- ALS_metrics.db$raster(ext = AOI, band = c(5, 15, 36))
plot(ALS_metrics.raster)


# ---- Postprocessing ---- #

#' reproject to coordinate system (if necessary):
ALS_metrics_prj.raster <- projectRaster(from = ALS_metrics.raster, crs = "EPSG:25833")

#' convert to terra raster object (to preserve the layer names):
ALS_metrics_prj.rast <- rast(ALS_metrics_prj.raster)

#' export raster stack to disk:
terra::writeRaster(ALS_metrics_prj.rast, "G:/ALS_metrics_2017_10m.tif", overwrite = T)



## 2. Query vector data ------------------------------------------------------------------------------------------------


#' Select vector database:
Areas_NPBW.db <- db$vectordb("Areas")

#' Get the polygons:
Areas_NPBW.poly <- Areas_NPBW.db$getVectors()

#' look at data:
Areas_NPBW.poly
mapview(Areas_NPBW.poly)



## 2. Query pointcloud data --------------------------------------------------------------------------------------------


### Large-scale ----


#' Queries point cloud data in large extents by splitting them into 1ha tiles

#' read AOI:
HTO_test <- read_sf("C:/Users/Rieser/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/HTO_test_areas.gpkg")

# Select a ALS acquisition
ALS_2017.db <- db$pointcloud("ALS_2017-06")

#' tiling the AOI into 1ha tiles:
HTO_test_tiles.poly <- st_make_grid(HTO_test, 100) %>% 
  st_as_sf()

# Querry the points

#' get the extent of each tile as the AOI:
AOI <- extent(HTO_test_tiles_poly[20,])

#' querry the points:
ALS_2017.points <- ALS_2017.db$points(ext=AOI)
head(ALS_2017.points)

#' redefine ScanAngleRank
ALS_2017.points$ScanAngleRank <- 0

#' convert dataframe to LAS:
ALS_2017.las <- RSDB::as.LAS(ALS_2017.points, proj4string = ALS_2017.db$proj4)

#' change the projection:
projection(ALS_2017.las)<- "EPSG:32632"

#' write to disk:
writeLAS(ALS_2017.las, "F:/ALS_2017.las")


### Small-scale ----


#' read AOI:
AOI <- read_sf("F:/Testdaten Befliegungen 2023/AOIs_Testdaten_Befliegungen.gpkg") %>% 
  filter(Gebiet == "Sulzschachten") %>% 
  extent()

# Select a ALS acquisition
ALS_2017.db <- db$pointcloud("ALS_2017-06")

#' querry the points:
ALS_2017.points <- ALS_2017.db$points(ext = AOI)
ALS_2017.points

#' remove unnecessary arguments
ALS_2017.points$scanAngleRank <- NULL
ALS_2017.points$classificationFlags <- NULL
head(ALS_2017.points)

#' convert dataframe to LAS:
ALS_2017.las <- RSDB::as.LAS(ALS_2017.points, proj4string = ALS_2017.db$proj4)

#' change the projection:
projection(ALS_2017.las)<- "EPSG:32632"
ALS_2017.las

#' write to disk:
writeLAS(ALS_2017.las, "F:/Testdaten Befliegungen 2023/2017/ALS_2017_Sulzschachten.las")



