library(lidR)
library(sf)
library(mapview)
library(tidyverse)
library(future)

## 1. Try different approaches -----------------------------------------------------------------------------------------



## 2.Implemented into Lascatalog ---------------------------------------------------------------------------------------

### 2.1. Make general setting for sf to be able to use the correct transformation method ----

#' check set up of sf package:
sf_proj_search_paths()
sf_extSoftVersion()

#' set options to be able to use transformation grids already implemented::
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
View(options)

#' select correct pipeline:
pipeline_BETA2007 <- options[1,]$definition
pipeline_BETA2007_own <- "+proj=utm +zone=32 +datum=ETRS89 +grids='D:/Reproject ALS Data test/Transformation grids/BETA2007/de_adv_BETA2007.tif'"

#' read Lascatalog:
ctg <- readALSLAScatalog("D:/Reproject ALS Data test/LiDAR GK")

#' check LAScatalog vailidity:
ctg
plot(ctg, mapview = T)
las_check(ctg)
st_crs(ctg) <- 31468

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:


opt_output_files(ctg) <- "D:/Reproject ALS Data test/LiDAR UTM/Catalog output/{ORIGINALFILENAME}_UTM_own"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = st_transform(las, crs = 25832, pipeline = pipeline_BETA2007_own)
  return(las_trans)
}

#' define parallel computation:
plan(multisession, workers = 6)

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)
