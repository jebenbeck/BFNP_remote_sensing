library(lidR)
library(sf)
library(mapview)
library(tidyverse)
library(future)

ctg <- readALSLAScatalog("D:/ALS 2012/Test")

x <- readALSLAS("D:/ALS 2012/Test2/spur00001_UTM32.laz")


#' check LAScatalog vailidity:
las_check(ctg)

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:
opt_output_files(ctg) <- "D:/ALS 2012/Test2/{ORIGINALFILENAME}_UTM32"
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
plan(multisession, workers = 2L)

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)


