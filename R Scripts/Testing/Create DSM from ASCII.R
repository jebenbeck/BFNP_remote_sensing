#' ----
#' Title: DEM preprocessing
#' Description: Generates a .tif raster mosaic from multiple ASCII DEM tiles
#' Author: Jakob Rieser
#' ----



# Setup ----

#' Packages needed:
Packages <- c("plyr", "tidyverse", "terra", "raster", "pbapply")
lapply(Packages, library, character.only = TRUE)

#' definition of input path. This is the folder holding the ASCII files
indir <- "E:/WIP/DOM/2019/ASCII"

#' definition of the outpot path. This is the location the DEM .tif raster will be exported to  
outdir <- "E:/WIP/DOM/2019/TIFF/"



# Processing ----

#' list of all ASCII datasets:
list_asc <- list.files(indir, full.names = T, pattern = "*.xyz")

#' function to read the ASCII data and convert it to raster::
convert_asc_tif <- function(list) {
  DEM_asc <- read.table(list, sep = " ")                                   #' import file:
  DEM_asc <- setNames (DEM_asc, c("longitude", "latitude", "altitude"))    #' set names
  DEM_raster <- raster::rasterFromXYZ(DEM_asc, crs = "EPSG:25832")         #' convert to raster
  return(DEM_raster)
  #writeRaster(DEM_raster, paste0(outdir, filename, ".tif"), overwrite = T) #' write raster to disk
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
rm(list_rast, list_rast_2)
gc()

#' make mosaic from raster collection:
terra::merge(raster_sprc, filename = paste0(outdir, "DSM_mosaic_2019.tif"), overwrite = T)
