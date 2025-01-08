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


### 1.1. Small datasets ----

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


### 1.2. Large datasets ----

#' sometimes, the data is too large to be stored or downloaded in a single *.tif file and should be downloaded and/or 
#' stored in tiles instead. The following code cuts the AOI into tiles and downloads them one by one. 

#' Convert the extent object to a SpatialPolygons and then to an sf object
AOI.sf <- as(AOI, "SpatialPolygons") %>% 
  st_as_sf(AOI.sp)

#' Set projection to match the source:
st_crs(AOI.sf) <- ALS_metrics.db$geo_code

#' Cutting the AOI into tiles of 1 km², any size can be chosen (distance of side length in mapping units): 
AOI_tiles.sf <- st_make_grid(AOI.sf, 1000) %>% 
  st_as_sf()

#' Function iterating through all tiles and downloading the respective data:
RSDB_query_raster_tiles <- function(i){
  #' make the respective tile new AOI:
  AOI <- raster::extent(AOI_tiles.sf[i,])
  #' Query the data:
  raster_data <- ALS_metrics.db$raster(ext = AOI, band = 3)
  #' convert to rast:
  return(rast(raster_data))
}

#' Applying the function with bar tracking the progress:
ALS_metrics_tiles.rast <- pblapply(1:nrow(AOI_tiles.sf), RSDB_query_raster_tiles)
ALS_metrics_tiles.rast

#' Merging the rasters:
ALS_metrics_merged.rast <- do.call(merge, ALS_metrics_tiles.rast)

#' Export to disk:
terra::writeRaster(ALS_metrics_merged.rast, "G:/ALS_metrics_2017_10m.tif", overwrite = T)



## 2. Query vector data ------------------------------------------------------------------------------------------------


#' Select vector database:
Areas_NPBW.db <- db$vectordb("Areas")

#' Get the polygons:
Areas_NPBW.poly <- Areas_NPBW.db$getVectors()

#' look at data:
Areas_NPBW.poly
mapview(Areas_NPBW.poly)



## 3. Query pointcloud data --------------------------------------------------------------------------------------------


### Large-scale ----

#' WARNING: NOT WORKING YET!

#' Queries point cloud data in large extents by splitting them into tiles

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