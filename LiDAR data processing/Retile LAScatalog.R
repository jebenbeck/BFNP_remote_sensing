## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2025-01-15
#' Status: Work in progress 


### Purpose of script ----

#' This script retiles lascatalogs, so many las files corresponding to a single acquisition. Sometimes, ALS las data is 
#' not stored in rigid tiles but in parts of flight lines which can overlap. This is not good, because if you want to 
#' know all points over a certain location within your AOI, you need to consider multiple files to get all of them.
#' 
#' This script takes any lascatalog (directory with many las/laz files) and restructures it to match a common grid of 
#' 1x1 km tiles in the UTM 32 system.

### Notes ----


### Required datasets ----


#' many las/laz files from ALS campaign

### Required packages ----

library(lidR)
library(sf)
library(tidyverse)
library(stringr)



## 1. Re-Tiling --------------------------------------------------------------------------------------------------------

ctg <- readALSLAScatalog("D:/ALS 2012/test")



#' check LAScatalog vailidity:
las_check(ctg)
plot(ctg, mapview = TRUE)


opt_chunk_alignment(ctg) <- c(0, 0)
opt_chunk_size(ctg) <- 1000
opt_chunk_buffer(ctg) <- 0
plot(ctg, mapview = T)
opt

opt_laz_compression(ctg) <- T
opt_output_files(ctg) <- paste0("D:/ALS 2012/test2", "/{XLEFT}_{YBOTTOM}") # label outputs based on coordinates
ctg_retiled <- catalog_retile(ctg) # apply retile

plot(ctg_retiled, mapview = T)
plot(ctg, mapview = T)


## 2. Post-processing spatial polygons of the lascatalog ---------------------------------------------------------------


#' postprocessing polygon data:
ctg_polygons <- st_as_sf(ctg_retiled) %>% 
  mutate(tile.name = str_extract(filename, "[^\\\\]+(?=\\.laz$)"), .before = everything()) %>% 
  select(-c(File.Signature, File.Source.ID, File.Creation.Year, File.Creation.Day.of.Year, GUID, Version.Major, Version.Minor, System.Identifier,
            Generating.Software, Header.Size, Offset.to.point.data, Number.of.variable.length.records, Point.Data.Format.ID, Point.Data.Record.Length,
            filename))

#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = "D:/ALS 2012/test2/tiles.gpkg", layer = "ALS_2012-06" , append = T)


### calculate point density of each file ----

opt_select(ctg_retiled) <- "xyz"

#' function to get point density of las data:
catalog_density = function(chunk) {
  las <- readLAS(chunk)
  if (is.empty(las)) return(NULL)
  density = density(las)
  return(density)
}

#' define parallel computation:
plan(multisession, workers = 2)

#' apply function to catalog:
density = catalog_apply(ctg_retiled, catalog_density)

test <- summary(ctg_retiled)
test
x <- density(ctg_retiled)