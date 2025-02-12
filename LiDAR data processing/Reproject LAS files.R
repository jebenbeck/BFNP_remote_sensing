library(lidR)
library(sf)
library(mapview)
library(future)

sf_proj_network(enable = T)
sf_proj_search_paths()
#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
options[1,]



## 1. LAScatalog map ---------------------------------------------------------------------------------------------------


#' set up parallel processing with 4 cores:
plan(multisession, workers = 4)

#' read Lascatalog:
ctg <- readALSLAScatalog("H:/Reproject ALS Data test/LiDAR GK/Originaldaten_subset")

#' apply epsg code:
st_crs(ctg) <- 31468

#' define output location and structure of catalog:
opt_output_files(ctg) <- "H:/Reproject ALS Data test/LiDAR UTM/test2/{ORIGINALFILENAME}_UTM"
opt_laz_compression(ctg) <- TRUE
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0
opt_independent_files(ctg) <- TRUE

#' check LAScatalog vailidity:
ctg
summary(ctg)
las_check(ctg)

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' function to reproject las data:
reproject_catalog = function(las)
{
  las_trans = sf::st_transform(las, crs = "EPSG:25832")
  return(las_trans)
}

#' apply function to catalog:
reprojected_ctg = catalog_map(ctg, reproject_catalog)


## 2. LAScatalog apply -------------------------------------------------------------------------------------------------


reproject_catalog_2 <- function(chunk){
  las <- readLAS(chunk)           #' read in the chunk
  if (is.empty(las)) return(NULL)    #' check if chunk actually contains points
  las_trans = sf::st_transform(las, crs = "EPSG:25832")      #' apply reprojection
  return(las_trans)
}

output <- catalog_apply(ctg, reproject_catalog_2)
