library(lidR)
library(sf)
library(mapview)
library(tidyverse)
library(future)

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
