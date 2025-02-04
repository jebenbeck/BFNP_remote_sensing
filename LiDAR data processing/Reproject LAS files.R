library(lidR)
library(sf)
library(mapview)
library(tidyverse)
library(future)


## Define transformation method ----------------------------------------------------------------------------------------

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
pipeline_BETA2007_own <- "+proj=utm +zone=32 +datum=ETRS89 +grids='F:/Reproject ALS Data test/Transformation grids/BETA2007/de_adv_BETA2007.tif'"
pipeline_KANU <- "+proj=utm +zone=32 +datum=ETRS89 +grids='F:/Reproject ALS Data test/Transformation grids/KANU/kanu_ntv2_niederbayern.tif'"

#' read Lascatalog:
ctg <- readALSLAScatalog("F:/Reproject ALS Data test/LiDAR GK")

#' check LAScatalog vailidity:
ctg
plot(ctg, mapview = T)
las_check(ctg)
st_crs(ctg) <- 31468

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:

opt_output_files(ctg) <- "F:/Reproject ALS Data test/LiDAR UTM/Test_Tower/{ORIGINALFILENAME}_UTM"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = sf::st_transform(las, crs = 25832, pipeline = pipeline_BETA2007)
  return(las_trans)
}

#' define parallel computation:
#plan(multisession, workers = 6)

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)



GK_single_file <- readLAS("F:/Reproject ALS Data test/LiDAR GK/2017_Trinkwassertalsperre.laz")

reprojected_single_file <- st_transform(GK_single_file, crs = 25832, pipeline = pipeline_BETA2007) 

writeLAS(reprojected_single_file, "F:/Reproject ALS Data test/LiDAR UTM/Test_Tower/test.laz")
