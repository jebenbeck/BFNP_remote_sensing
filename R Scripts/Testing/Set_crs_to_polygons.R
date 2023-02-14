library(sf)
library(parallel)
library(doParallel)
library(foreach)



files <- list.files("F:/Einzelbaumpolygone 2017/Original/", pattern = "*.shp$", recursive = T, full.names = TRUE)
filenames <- basename(list.files("F:/Einzelbaumpolygone 2017/Original/", pattern = "*.shp$", recursive = T, full.names = TRUE))


#' Set up the parallel computation: 
no_cores <- 8   #' number of cores
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)


foreach(i = 1:length(files), .packages = "sf") %dopar% {
  #' read polygons
  polygons <- read_sf(files[[i]], quiet = T)
  #' apply projection
  polygons_prj <- st_set_crs(polygons, st_crs("EPSG:31468"))
  #' export as gpkg:
  st_write(polygons_prj, dsn = paste0("F:/Einzelbaumpolygone 2017/Projected/", filenames[[i]], ".gpkg"), driver = "GPKG", append = F)
  rm(polygons)
  rm(polygons_prj)
}

stopCluster(cl)
