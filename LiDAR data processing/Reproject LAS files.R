library(lidR)
library(sf)
library(mapview)
library(tidyverse)

## 1. Try different approaches -----------------------------------------------------------------------------------------

LAS_GK <- readALSLAS("H:/Reproject ALS Data test/Base data/2017_AOI.laz")
LAS_GK

#' Ckech set up of sf package:
sf_proj_search_paths()
sf_extSoftVersion()

#' set options to be able to use traansformation grids already implemented::
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
View(options)

#' select correct pipeline:
pipeline_BETA2007 <- "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=hgridshift +grids=de_adv_BETA2007.tif +step +proj=utm +zone=32 +ellps=GRS80"

#' apply transformation:
LAS_UTM_sf_BETA2007 <- st_transform(LAS_GK, crs = 25832, pipeline = pipeline_BETA2007)

#' export new transformed laz file:
writeLAS(LAS_UTM_sf_BETA2007, "H:/Reproject ALS Data test/Results/2017_AOI_UTM_sf_BETA2007_new.laz")






## 2.Implemented into Lascatalog ---------------------------------------------------------------------------------------

ctg <- readALSLAScatalog("F:/ALS 2012/Punktwolken_laz")

#' check LAScatalog vailidity:
las_check(ctg)

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:
opt_output_files(ctg) <- "F:/ALS 2012/UTM32/{ORIGINALFILENAME}_UTM"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = st_transform(las, "epsg:25832")
  return(las_trans)
}

#' define parallel computation:
plan(multisession, workers = 6)

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)


x <- readLAS("F:/ALS 2012/UTM32/spur00001_UTM.laz")
x@data
x
