# Header --------------------------------------------
# 
# Original Author: Jakob Rieser, jakob.rieser@npv-bw.bayern.de
# Created on: 2023-02-21
# 
# Last modified by:
# On:
# 
# Script Description: Benchmarking different functions used to extract raster data for complex polygons. 
# Hereby, exactextractr is significantly quicker than methods implemented in the raster and terra packages.



require(geodata)
require(terra)
require(exactextractr)
require(sf)
require(microbenchmark)
require(ggplot2)



# Get data ----

#' Pull municipal boundaries for Germany
#' It's a complex polygon shapefile with 403 (multi-)polygons
germany.sf <- geodata::gadm(country = "DE", level = 2, path = tempdir()) %>% 
  st_as_sf()

plot(germany.sf["NAME_2"])

#' Pull gridded bioklim global precipitation data
precipitation.raster <- geodata::worldclim_global(var='prec', res=10, path = tempdir())

plot(precipitation.raster$wc2.1_10m_prec_01)



# Benchmark ----

#' Calculate the mean precipitation in December for all municipalities using the different packages 5 times each and 
#' perform benchmarks (takes a few seconds)

results_bm <- microbenchmark(
  #' using the exactextractr package:
  result_exaxtextractr = exactextractr::exact_extract(precipitation.raster[[12]], germany.sf, "mean", progress = F),
  #' using the raster package:
  result_raster = raster::extract(precipitation.raster[[12]], germany.sf, mean, na.rm=TRUE),
  #' using the terra package:
  result_terra = terra::extract(precipitation.raster[[12]], germany.sf, mean, na.rm=TRUE),
  #' repeat 5 times:
  times = 5
)

#' Visualize the computation time of the different functions:
autoplot(results_bm)
