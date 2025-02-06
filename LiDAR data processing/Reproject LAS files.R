library(lidR)
library(sf)
library(mapview)
library(future)
library(pbapply)

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


#' set up parallel processing with 6 cores:
plan(multisession, workers = 4)
plan(sequential)
lidR::set_lidr_threads(4L)

#' read Lascatalog:
ctg <- readALSLAScatalog("F:/Reproject ALS Data test/LiDAR GK/Originaldaten_subset")

#' apply epsg code:
st_crs(ctg) <- 31468

#' check LAScatalog vailidity:
ctg
summary(ctg)
las_check(ctg)

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and structure of catalog:
opt_output_files(ctg) <- "F:/Reproject ALS Data test/LiDAR UTM/Output Catalog Originaldaten Subset parralel 2/{ORIGINALFILENAME}_UTM"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = sf::st_transform(las, crs = 25832, pipeline = pipeline_BETA2007)
  return(las_trans)
}

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)




## 2. pbapply -------------------------------------------------------------------------------------------------------

#' Use pbapply for processing all ASC files in parallel:

reproject_files <- function(las_file){
  
  #' read laz file:
  las_data <- readALSLAS(las_file)
  
  #' Extract the filename without the path and extension
  file_name <- tools::file_path_sans_ext(basename(las_file))
  
  #' set crs:
  st_crs(las_data) <- 31468
  
  #' transform las data:
  las_trans = sf::st_transform(las_data, crs = 25832, pipeline = pipeline_BETA2007)
  
  #' export the LAS object to a LAS file
  output_file <- file.path(output_dir, paste0(file_name, "_UTM", ".laz"))
  writeLAS(las_data, output_file)
  
}

output_dir <- "F:/Reproject ALS Data test/LiDAR UTM/Output Catalog Originaldaten Subset parralel pbapply/"

laz_files <- list.files("F:/Reproject ALS Data test/LiDAR GK/Originaldaten_subset", pattern = "\\laz$", full.names = TRUE)

cluster <- parallel::makeCluster(4)

parallel::clusterExport(cluster, varlist = c("readLAS", "writeLAS", "basename", "output_dir", "st_crs", "st_transform", "pipeline_BETA2007"))
parallel::clusterEvalQ(cluster, library(lidR)) # Load lidR on all nodes
parallel::clusterEvalQ(cluster, library(sf)) # Load tidyverse on all nodes


output_files <- pblapply(laz_files, reproject_files, cl = cluster)

parallel::stopCluster(cluster)

test <- readALSLAS("F:/Reproject ALS Data test/LiDAR UTM/Output Catalog Originaldaten Subset parralel 2/NPV_00130_UTM.laz")
plot(test)
