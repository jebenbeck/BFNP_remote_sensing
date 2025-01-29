library(lidR)
library(sf)
library(mapview)
library(tidyverse)
library(future)

## 1. Try different approaches -----------------------------------------------------------------------------------------

LAS_GK_Trinkwassertalsperre <- readALSLAS("H:/Reproject ALS Data test/LiDAR GK/2017_Trinkwassertalsperre.laz")
LAS_GK_Waldhäuser <- readALSLAS("H:/Reproject ALS Data test/LiDAR GK/2017_Waldhäuser.laz")
plot(LAS_GK_Trinkwassertalsperre)

#' check set up of sf package:
sf_proj_search_paths()
sf_extSoftVersion()

#' set options to be able to use transformation grids already implemented::
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
View(options)


#' select correct pipeline:

#' 1) BETA 2007 transformation:
pipeline_BETA2007 <- options[1,]$definition
pipeline_BETA2007 <- options[1,]$definition

#' 2) KANU transformation (Bavaria only)
pipeline_Kanu <- "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=hgridshift +grids=H:/Reproject ALS Data test/Transformation grids/KANU/kanu_ntv2_niederbayern.tif +step +proj=utm +zone=32 +ellps=GRS80"



#' apply transformation:
LAS_UTM_Trinkwassertalsperre_BETA2007 <- st_transform(LAS_GK_Trinkwassertalsperre, crs = 25832, pipeline = pipeline_BETA2007)
LAS_UTM_Trinkwassertalsperre_Kanu <- st_transform(LAS_GK_Trinkwassertalsperre, crs = 25832, pipeline = pipeline_Kanu)

LAS_UTM_Waldhäuser_BETA2007 <- st_transform(LAS_GK_Waldhäuser, crs = 25832, pipeline = pipeline_BETA2007)
LAS_UTM_Waldhäuser_Kanu <- st_transform(LAS_GK_Waldhäuser, crs = 25832, pipeline = pipeline_Kanu)


#' export new transformed laz file:
writeLAS(LAS_UTM_Trinkwassertalsperre_BETA2007, "H:/Reproject ALS Data test/LiDAR UTM/LAS_UTM_Trinkwassertalsperre_BETA2007.laz" )
writeLAS(LAS_UTM_Trinkwassertalsperre_Kanu, "H:/Reproject ALS Data test/LiDAR UTM/LAS_UTM_Trinkwassertalsperre_Kanu.laz" )

writeLAS(LAS_UTM_Waldhäuser_BETA2007, "H:/Reproject ALS Data test/LiDAR UTM/LAS_UTM_Waldhaeuser_BETA2007.laz" )
writeLAS(LAS_UTM_Waldhäuser_Kanu, "H:/Reproject ALS Data test/LiDAR UTM/LAS_UTM_Waldhaeuser_Kanu.laz" )



#' lidar GK

LAS_GK <- readALSLAS("H:/Reproject ALS Data test/LiDAR GK/NPV_01266.laz")
st_crs(LAS_GK) <- 31468
LAS_GK

AOI_GK <- st_read("H:/Reproject ALS Data test/AOI.gpkg", layer = "AOI_Lusen_GK")
AOI_GK

LAS_GK_AOI <- lidR::clip_roi(LAS_GK, st_geometry(AOI_GK))
plot(LAS_GK_AOI)

writeLAS(LAS_GK_AOI, "H:/Reproject ALS Data test/LiDAR GK/2017_Lusen.laz")


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

#' read Lascatalog:
ctg <- readALSLAScatalog("H:/Reproject ALS Data test/LiDAR GK")

#' check LAScatalog vailidity:
ctg
plot(ctg, mapview = T)
las_check(ctg)
st_crs(ctg) <- 31468

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:


opt_output_files(ctg) <- "H:/Reproject ALS Data test/LiDAR UTM/Catalog output/{ORIGINALFILENAME}_UTM"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = st_transform(las, crs = 25832, pipeline = pipeline_BETA2007)
  return(las_trans)
}

#' define parallel computation:
plan(multisession, workers = 6)

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)
