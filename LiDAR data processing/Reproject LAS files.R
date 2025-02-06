library(lidR)
library(sf)
library(mapview)

#' check set up of sf package:
sf_proj_search_paths()
sf_extSoftVersion()
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")

#' select correct pipeline:
pipeline_BETA2007 <- options[1,]$definition
pipeline_BETA2007



## 1. LAScatalog -------------------------------------------------------------------------------------------------------


#' read Lascatalog:
ctg <- readALSLAScatalog("F:/Reproject ALS Data test/LiDAR GK")

#' apply epsg code:
st_crs(ctg) <- 31468

#' check LAScatalog vailidity:
ctg
las_check(ctg)

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:
opt_output_files(ctg) <- "F:/Reproject ALS Data test/LiDAR UTM/Output Catalog PC2/{ORIGINALFILENAME}_UTM"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = sf::st_transform(las, crs = 25832, pipeline = pipeline_KANU)
  return(las_trans)
}

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)



## 2. Single file ------------------------------------------------------------------------------------------------------


#' read single file:
GK_single_file <- readLAS("F:/Reproject ALS Data test/LiDAR GK/2017_Zwieslerwaldhaus.laz")

#' reproject:
reprojected_single_file <- sf::st_transform(GK_single_file, crs = 25832, pipeline = pipeline_BETA2007) 

#' export to disk:
writeLAS(reprojected_single_file, "F:/Reproject ALS Data test/LiDAR UTM/Output Single PC1/2017_Zwieslerwaldhaus_UTM.laz")
