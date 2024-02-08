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
opt_output_files(ctg) <- "E:/Work in Progress/Classification Output/{ORIGINALFILENAME}_classified"

#' define parallel computation:
plan(multisession, workers = 3L)

#' classify ground points:
out <- classify_ground(ctg, algorithm = csf(), last_returns = T)
