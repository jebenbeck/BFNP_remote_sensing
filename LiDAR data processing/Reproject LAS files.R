library(lidR)
library(sf)
library(mapview)
library(future)


## LAScatalog function -------------------------------------------------------------------------------------------------


#' This is a function to transform the coordinate system of a lascatalog. It is specialized for large-scale ALS data (many flight lines / tiles).
#' While it can transform from/to any system implemented in st_transform, it is specialized for converting GK4 to UTM32
#' by using transformation grid from the German surveying authorities. Input shall be a las catalog (folder with many LAZ/LAS files), 
#' output will be LAZ files. The results will be the exact same files but reprojected to the correct epsg code. 
#' There will be no retiling or alterations of chunks.


reproject_lascatalog <- function(lascatalog, input_epsg, output_epsg, output_path, parallel = FALSE, n_cores = 2)  {
  
  #' check projection if needed:
  if (is.na(st_crs(lascatalog))) {
    warning(paste("LasCatalog does not have an assigned projection, input_epsg", input_epsg, "will be used"), call. = F, immediate. = T)
    st_crs(lascatalog) <- input_epsg
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
    message("Proj connection to network enabled:", sf_proj_network())
    #' get transformation options available with grid:
    options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
    message ("Transformation option in use:", options[1,]$definition)
    #' ckech if transformation grid exists:
    if (file.exists(paste0(sf_proj_search_paths(), "/de_adv_BETA2007.tif")) == T) {
      message ("BETA 2007 transformation grid found and will be used")
      } else {
        warning("BETA 2007 transformation grid not found, accurracy of transformation will be compromized", call. = F, immediate. = T)
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
    message("Parallel processing will be used with", ncores, "cores")
  } else {
    warning("No parallel processing in use", call. = F, immediate. = T)
  }
  
  #' apply function to lascatalog:
  reprojected_catalog = catalog_map(lascatalog, reproject)
  return(reprojected_catalog)
}

### Apply function ----

#' read Lascatalog:
ctg <- readALSLAScatalog("D:/Reproject ALS Data test/LiDAR GK/Originaldaten_subset")

#' check LAScatalog vailidity:
ctg
summary(ctg)
las_check(ctg)

#' plot LAScatalog:
plot(ctg)

#' apply the transformation:
ctg_UTM32 <- reproject_lascatalog(lascatalog = ctg,
                     input_epsg = "EPSG:31468",
                     output_epsg = "EPSG:25832", 
                     output_path = "D:/Reproject ALS Data test/LiDAR UTM/test3")
