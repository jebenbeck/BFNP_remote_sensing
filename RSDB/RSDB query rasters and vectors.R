library(RSDB)
library(sf)
library(raster)
library(terra)
library(mapview)
library(tidyverse)
library(lidR)
library(pbapply)



# ---- Connect to RSDB server ---- #

#' Provide the login-credentials in an local R file on your computer or via an object (format: "username:password")
source("C:/Users/jakob/OneDrive/BFNP/Documents/RSDB/RSDB credentials.R")

#' Connect to the server
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 



## 1. Query raster data ------------------------------------------------------------------------------------------------


### 1.1. Small datasets ----


# ---- Get raster data ---- #

#' Select raster database:
ALS_metrics.db <- db$rasterdb("LiDAR_metrics_ALS_2023_07_10m")

#' define AOI (can be any spatial object)
#' in this case, it is the full extent of the data
AOI <- ALS_metrics.db$extent

#' query rasterstack of ALS Metrics: 
ALS_metrics.raster <- ALS_metrics.db$raster(ext = AOI, band = c(5, 6))


# ---- Postprocessing ---- #

#' convert to terra raster object (to preserve the layer names):
ALS_metrics.rast <- rast(ALS_metrics.raster)

#' reproject/assign coordinate system (if necessary):
terra::crs(ALS_metrics.rast) <- ALS_metrics.db$geo_code

#' export raster stack to disk:
terra::writeRaster(ALS_metrics.rast, "C:/Users/jakob/Desktop/temp/ALS_metrics_2023_10m.tif", overwrite = T)



### 1.2. Large datasets ----


#' sometimes, the data is too large to be stored or downloaded in a single *.tif file and should be downloaded and/or 
#' stored in tiles instead. The following function cuts the AOI into tiles and downloads them one by one. 

#' definition of inputs:
#' 1) rasterdb_name is the character name of the rasterdb that should be downloaded
#' 2) AOI can be the character string "full" which returns the full extent of the rasterdb or a spatial polygon 
#' object (type st/sf) resulting from gpkg or shp files. In this case the path to the file needs to be pasted here.
#' 3) tilesize is the length of the tile sides in x and y directions corresponding to mapping units (usually meters)
#' 4) bands can be one number or a vector of numbers corresponding to the bands/layers of the rasterdb that should be 
#'    downloaded 

RSDB_query_raster_tiles <- function(rasterdb_name, AOI, tilesize, bands) {
  
  #' get the rasterdatabase:
  rasterdatabase <- db$rasterdb(rasterdb_name)
  
  # get area of interest in correct format:
  get_area_of_interest <- function(AOI, rasterdatabase) {
    if (AOI == "full") {
      # Return the extent of the raster database
      areaofinterest <- as(rasterdatabase$extent, "SpatialPolygons") %>% 
        st_as_sf(AOI.sp)
      st_crs(areaofinterest) <- rasterdatabase$geo_code
    } else {
      # Otherwise, assume AOI is a polygon object (sf object)
      areaofinterest <- st_read(AOI, quiet = T)
    }
    
    return(areaofinterest)
  }
  
  area_of_interest <- get_area_of_interest(AOI, rasterdatabase)
  
  #' Cutting the AOI into tiles of 1 km², any size can be chosen (distance of side length in mapping units): 
  tiles <- st_make_grid(area_of_interest, tilesize) %>% 
    st_as_sf()
  
  print(paste0("tiling successfull, ", nrow(tiles), " tiles generated"))
  
  #' Function iterating through all tiles and downloading the respective data:
  query_tiles <- function(i){
    #' make the respective tile new AOI:
    area_of_interest_tile <- raster::extent(tiles[i,])
    #' Query the data:
    raster_data <- rast(rasterdatabase$raster(ext = area_of_interest_tile, band = bands))
    #' set coordinate system:
    terra::crs(raster_data) <- rasterdatabase$geo_code
    
    return(raster_data)
  }
  
  print("starting downloading tiles from RSDB server...")
  
  #' Applying the function with bar tracking the progress:
  raster_tiles <- pblapply(1:nrow(tiles), query_tiles)
  
  print("download successfull")
  
  return(raster_tiles)
  
}

#' run function to receive list of rasters in memory:
raster_list <- RSDB_query_raster_tiles (rasterdb_name = "LiDAR_metrics_ALS_2023_07_10m", AOI = "full", 
                                        tilesize = 1000, bands = 3)

#' Export the tiles to disk as seperate tif files:
pblapply(seq_along(raster_list), function(i) {
  # Construct the filename
  filename <- file.path("C:/Users/NBW-Ebenbeck_J/Downloads/", paste0("LiDAR_metrics_2023_07_10m_", i, ".tif"))
  
  # Export the raster object to a .tif file
  terra::writeRaster(raster_list[[i]], filename, overwrite = TRUE)
})

#' Merging the tiles to single raster file:
raster_merged <- do.call(merge, raster_tiles)

#' Export the merged raster to tif:
terra::writeRaster(raster_list[[i]], filename, overwrite = TRUE)



## 2. Query vector data ------------------------------------------------------------------------------------------------


#' Select vector database:
Areas_NPBW.db <- db$vectordb("Areas")

#' Get the polygons:
Areas_NPBW.poly <- Areas_NPBW.db$getVectors()

#' look at data:
Areas_NPBW.poly
mapview(Areas_NPBW.poly)



## 3. Query pointcloud data --------------------------------------------------------------------------------------------


#' read AOI:
AOI.sf <- read_sf("H:/Testdaten Befliegungen/AOIs_Testdaten_Befliegungen.gpkg") %>% 
  filter(Gebiet == "Sulzschachten") 

mapview(AOI.sf)

#' make extent object:
AOI <- extent(AOI.sf)

# Select a ALS acquisition
ALS_2017.db <- db$pointcloud("ALS_2017-06")

#' querry the points:
ALS_2017.points <- ALS_2017.db$points(ext = AOI)

#' check the data:
str(ALS_2017.points)

#' remove unnecessary arguments
ALS_2017.points$scanAngleRank <- NULL #' needed because LAS standard can not deal with the way it is stored
ALS_2017.points$classificationFlags <- NULL
head(ALS_2017.points)

#' convert dataframe to LAS:
ALS_2017.las <- RSDB::as.LAS(ALS_2017.points)
ALS_2017.las

#' assign the projection:
projection(ALS_2017.las) <- ALS_2017.db$geocode
ALS_2017.las

#' take a look at the data:
plot(ALS_2017.las)

#' export to disk:
writeLAS(ALS_2017.las, "C:/Users/jakob/Desktop/temp/ALS_2017_Sulzschachten.las")
