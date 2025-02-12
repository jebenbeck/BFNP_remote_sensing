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



## 2. LAScatalog function -------------------------------------------------------------------------------------------------

#' This is a function to transform the coordinates. It is specialized for large-scale ALS data (many flight lines / tiles)
#' and the conversion from GK4 to UTM32. Input shall be LAZ/LAS files, output shall be LAZ files. The results will be the 
#' exact same files but reprojected to the correct epsg code. There will be no retiling or alterations of chunks.


reproject_lascatalog <- function(lascatalog, input_epsg, output_epsg, output_path, parallel, n_cores)  {
  
  #' check projection if needed:
  if (is.na(st_crs(lascatalog))) {
    stop("Las catalog does not have an assigned projection")
  }
  
  #' apply options to lascatalog
  opt_output_files(lascatalog) <- paste0(output_path, "/{ORIGINALFILENAME}_transformed")
  opt_laz_compression(lascatalog) <- TRUE
  opt_chunk_buffer(lascatalog) <- 0
  opt_chunk_size(lascatalog) <- 0
  opt_independent_files(lascatalog) <- TRUE
  
  #' special check if transformation is from GK4 to UTM32
  if (input_epsg == "EPSG:31468" & output_epsg == "EPSG:25832") {
    #' activate network connection
    sf_proj_network(enable = T)
    print(paste("Proj connection to network enabled:", sf_proj_network()))
    #' get transformation options available with grid:
    options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
    print(paste("Transformation option in use:", options[1,]$definition))
    #' ckech if transformation grid exists:
    if (file.exists(paste0(sf_proj_search_paths(), "/de_adv_BETA2007.tif")) == T) {
      print("BETA 2007 transformation grid found and will be used")
      } else {
        print("BETA 2007 transformation grid not found, accurracy of transformation will be compromized")
      }
    }
  
  #' function to reproject las data:
  reproject = function(las){
    las_trans = sf::st_transform(las, crs = output_epsg)
    return(las_trans)
  }
  
  #' plan parallel processing
  if (parallel == TRUE) {
    plan(multisession, workers = n_cores)
  }
  
  #' apply function to lascatalog:
  reprojected_ctg = catalog_map(lascatalog, reproject)
  return(reprojected_catalog)
}

#' read Lascatalog:
ctg <- readALSLAScatalog("D:/Reproject ALS Data test/LiDAR GK/Originaldaten_subset")

#' apply epsg code:
st_crs(ctg) <- 31468

#' check LAScatalog vailidity:
ctg
summary(ctg)
las_check(ctg)
is.na(st_crs(ctg))

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' apply the transformation:
reproject_lascatalog(lascatalog = ctg,
                     input_epsg = "EPSG:31468",
                     output_epsg = "EPSG:25832", 
                     output_path = "D:/Reproject ALS Data test/LiDAR UTM/test2", 
                     parallel = F, n_cores = 4)

