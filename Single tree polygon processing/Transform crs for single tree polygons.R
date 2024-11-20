library(sf)
library(parallel)
library(doParallel)
library(foreach)



files <- list.files("D:/Single tree polygons 2017/Projected_GK/", pattern = "*.gpkg$", recursive = T, full.names = TRUE)
filenames <- basename(files)


#' Set up the parallel computation: 
no_cores <- 8   #' number of cores
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)


foreach(i = 1:length(files), .packages = "sf") %dopar% {
  #' read polygons
  polygons <- read_sf(files[[i]], quiet = T)
  #' convert projection to UTM:
  polygons_prj <- st_transform(polygons, crs = st_crs("EPSG:25832"))
  #' export as gpkg:
  st_write(polygons_prj, dsn = paste0("D:/Single tree polygons 2017/Projected_UTM/", filenames[[i]]), driver = "GPKG", append = F)
  rm(polygons)
  rm(polygons_prj)
}

stopCluster(cl)
