## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Rieser
#' Last updated: 2023-02-15
#' Status: Final 


### Purpose of script ----

#' Loops over multiple ASCII tiles holding DEM information, converts them to raster and mosaics them


### Required datasets ----

#' - Multiple ASCII files holding geolocated DEM information


### Required packages ----

require(plyr)
require(tidyverse)
require(terra)
require(raster)
require(pbapply)


### Set directories ----

#' definition of input path. This is the folder holding the ASCII files
indir <- "D:/DOM Luftbilder 2015-2021/ASCII"

#' definition of the outpot path. This is the location the.tif raster will be exported to  
outdir <- "D:/DOM Luftbilder 2015-2021/TIFF"



## 1. Processing -------------------------------------------------------------------------------------------------------


#' make a list of all ASCII datasets:
list_asc <- list.files(indir, full.names = T, pattern = "*.xyz")

#' make function to read the ASCII data and convert it to raster::
convert_asc_tif <- function(list) {
  #' import respective ASCII file:
  DEM_asc <- read.table(list, sep = " ")
  
  #' set names to the columns in the data:
  DEM_asc <- setNames (DEM_asc, c("longitude", "latitude", "altitude"))
  
  #' convert to raster
  DEM_raster <- raster::rasterFromXYZ(DEM_asc, crs = "EPSG:25832")
  
  return(DEM_raster)
  
  #' clean environment to avoid memory issues
  rm(c(DEM_asc, DEM_raster))
  gc()
}

#' apply function in parallel:
cluster <- parallel::makeCluster(7)
list_rast <- pblapply(list_asc, convert_asc_tif, cl= cluster)
parallel::stopCluster(cluster)

#' convert rasters to spatRaster to be used in terra:
list_rast_2 <- pblapply(list_rast, rast)

#' convert to a spatial raster collection:
raster_sprc <- sprc(list_rast_2)

#' clean environment to avoid memory issues
rm(list_rast, list_rast_2)
gc()

#' make mosaic from raster collection:
terra::merge(raster_sprc, filename = paste0(outdir, "DSM_mosaic_2019.tif"), overwrite = T)