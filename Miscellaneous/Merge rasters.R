library(terra)
library(raster)

#' load in list of rasters:
raster_files <- list.files("E:/WIP/DOM/2020/TIFF", pattern = "*.tif$", recursive = T, full.names = TRUE)

#' load in rasters:
raster_list <- lapply(raster_files, rast)

#' convert to a spatial raster collection:
raster_sprc <- sprc(raster_list)

#' make mosaic from raster collection:
terra::merge(raster_sprc, filename = paste0("E:/WIP/DOM/2020/TIFF/", "DSM_mosaic_2020.tif"), overwrite = T)