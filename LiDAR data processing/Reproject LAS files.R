library(lidR)
library(sf)
library(mapview)
library(tidyverse)
library(future)

## 1. Try different approaches -----------------------------------------------------------------------------------------

LAS_GK <- readALSLAS("D:/Reproject ALS Data test/Base data/2017_AOI.laz")
plot(LAS_GK)


#' convert using sf default without network connection:
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
View(options)



LAS_UTM_sf_default <- st_transform(LAS_GK, crs = 25832)
writeLAS(LAS_UTM_sf_default, "D:/Reproject ALS Data test/Results/2017_AOI_UTM_sf_default.laz")

#' set options to be able to use treansformation grids already implemented::
sf_proj_network(enable = T)
sf_proj_search_paths()
sf_extSoftVersion()


#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832", grid_availability = "AVAILABLE")
View(options)


#' select the pipeline using official BETA2007 transformation grid:
LAS_UTM_sf_BETA2007 <- st_transform(LAS_GK, crs = 25832, pipeline = "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=hgridshift +grids=D:/Reproject ALS Data test/Transformation grids/de_adv_BETA2007.tif +step +proj=utm +zone=32 +ellps=GRS80")

DF_GK <- st_as_sf(LAS_GK)


writeLAS(LAS_UTM_sf_BETA2007, "D:/Reproject ALS Data test/Results/LAS_UTM_sf_BETA2007.laz")

LAS_GK@data
test@data

sf_extSoftVersion()
#' convert using sf package:
DF_GK <- st_as_sf(LAS_GK)

DF_UTM <- sf::st_transform(DF_GK, crs = 25832)

DF_UTM_2 <- sf::st_transform(DF_GK, pipeline = "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=hgridshift +grids=de_adv_BETA2007.tif +step +proj=utm +zone=32 +ellps=GRS80")
DF_UTM_2 <- sf::st_transform(DF_GK, pipeline = "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=utm +zone=32 +ellps=GRS80")
DF_UTM


coordinates <- st_coordinates(DF_UTM$geometry)
DF_UTM$X <- coordinates[, "X"]
DF_UTM$Y <- coordinates[, "Y"]
DF_UTM$Z <- coordinates[, "Z"]

DF_UTM <- st_drop_geometry(DF_UTM)

LAS_UTM_SF <- LAS(DF_UTM)
LAS_UTM_SF
st_crs(LAS_UTM_SF) <- 25832

writeLAS(LAS_UTM_SF, "D:/ALS 2017/Test_conversion/LAS_UTM_SF.laz")

test_UTM_2 <- st_transform(test, crs = "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=utm +zone=32 +ellps=GRS80")
test_UTM_2 <- writeLAS(test_UTM, "D:/ALS 2023/2017_AOI_prj_st_datum_new.laz") 

x <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832", desired_accuracy = 0.1, grid_availability = "AVAILABLE")
sf_proj_network(enable = T)
x$description
x$definition


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
