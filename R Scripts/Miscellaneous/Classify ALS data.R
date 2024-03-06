require(sf)
require(lidR)
require(tidyverse)
require(future)

#' Make LAScatalog object:
ctg <- readLAScatalog("E:/02_Punktwolke")

#' check LAScatalog vailidity:
las_check(ctg)

#' plot LAScatalog:
plot(ctg, mapview = TRUE)

#' define output location and file structure:
opt_output_files(ctg) <- "C:/ALS Data/Classification Output/{ORIGINALFILENAME}_classified"
opt_laz_compression(ctg) <- TRUE

#' define parallel computation:
plan(multisession, workers = 2L)

#' classify ground points:
out <- classify_ground(ctg, algorithm = csf(), last_returns = T)
