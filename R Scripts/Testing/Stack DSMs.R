library(terra)
library(pbapply)
library(tidyverse)
library(sf)

#' list of all ASCII datasets:
list_DSM <- list.files("E:/WIP/DOM", recursive = T, full.names = T, pattern = "^DSM_mosaic_")

list_rast <- pblapply(list_DSM, rast)
list_rast

#' reproject all rasters:
project_rast <- function(list) {
  return(terra::project(x = list, y = "EPSG:25832", align = T))
}

list_ras_res <- pblapply(list_rast, project_rast)

#' get overall extent of rasters:
extent_rasters <- lapply(list_ras_res, ext) %>% 
  lapply(as.polygons) %>%
  lapply(st_as_sf) %>%
  bind_rows() %>%
  st_union() %>% 
  st_set_crs(25832) %>% 
  vect() %>% 
  ext()

#' make empty raster based on extents of the rasters:
ref_rast <- terra::rast(crs = "EPSG:25832", extent = extent_rasters, resolution = 1)

#' resample all rasters:
resample_rast <- function(list) {
  return(terra::resample(x = list, y = ref_rast))
}

list_ras_res_2 <- pblapply(list_ras_res, resample_rast)

#' stack the datasets:
stack_ras <- rast(list_ras_res_2)

#' change layer names:
names(stack_ras) <- c("DSM_2015-06", "DSM_2016-06", "DSM_2017-06", "DSM_2018-07", "DSM_2019-06", "DSM_2020-08", "DSM_2021-06")
names(stack_ras)

#' export to disk:
terra::writeRaster(stack_ras, filename = paste0("E:/WIP/DOM/", "DSM_stack_2015-2021.tif"), overwrite = T)
