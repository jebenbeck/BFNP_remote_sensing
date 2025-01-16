# this is just a test 


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
library(mapview)



## 1. Re-Tiling --------------------------------------------------------------------------------------------------------

ctg <- readALSLAScatalog("D:/ALS 2012/test")



#' check LAScatalog vailidity:
las_check(ctg)
summary(ctg)
plot(ctg, mapview = TRUE)

#' set catalog options:
opt_chunk_alignment(ctg) <- c(0, 0) #' define tile alignment
opt_chunk_size(ctg) <- 1000         #' define tile size
opt_chunk_buffer(ctg) <- 0          #' define buffer size
opt_laz_compression(ctg) <- T       #' define outpt as LAZ
opt_output_files(ctg) <- paste0("D:/ALS 2012/test2", "/{XLEFT}_{YBOTTOM}") #' label outputs based on coordinates

#' perform the retiling:
ctg_retiled <- catalog_retile(ctg) 

#' have a look at the new tiles:
plot(ctg_retiled, mapview = T)


## 2. Post-processing spatial polygons of the lascatalog ---------------------------------------------------------------

#' postprocessing polygon data:
catalog_to_polygons <- function(clg) {
  ctg_polygons <- st_as_sf(clg) %>% 
    #' convert filename to tile name:
    mutate(Tile.name = str_extract(filename, "[^\\\\]+(?=\\.laz$)"), .before = everything()) %>% 
    #' remove unecessary attributes:
    select(-c(File.Signature, File.Source.ID, File.Creation.Year, File.Creation.Day.of.Year, GUID, Version.Major, Version.Minor, System.Identifier,
              Generating.Software, Header.Size, Offset.to.point.data, Number.of.variable.length.records, Point.Data.Format.ID, Point.Data.Record.Length,
              filename)) %>% 
    #' rename number of points:
    rename("Number.of.points" = "Number.of.point.records") %>% 
    relocate(CRS, .after = Tile.name) %>% 
    relocate(Number.of.points, .before = Number.of.1st.return)
  
  return(ctg_polygons)
}

#' apply function:
ctg_retiled_polygons <- catalog_to_polygons(ctg_retiled)


#' export polygon file to geopackage:
st_write(ctg_polygons, dsn = "D:/ALS 2012/test2/tiles.gpkg", layer = "ALS_2012-06" , append = T)


## 3. Calculate point density of new tiles -----------------------------------------------------------------------------


#' set catalog options:
opt_select(ctg_retiled) <- "xy" #' read only xy because they are the only attributes of interest
opt_output_files(ctg_retiled) <- "" #' receive results in R environment 

#' function to get point density of las data:
catalog_file_stats = function(chunk) {
  #return(chunk)
  las <- readLAS(chunk)
  if (is.empty(las)) return(NULL)
  #' calculate point density:
  density = density(las) 
  #' calculate area covered by convex hull (not bbox):
  area = area(las)
  #' extract tile name from file (for merging information to other data later):
  tilename = str_extract(chunk@files, "[^\\\\]+(?=\\.laz$)")
  #' extract extent of full tile:
  extent_tile <- chunk@bbox
  #' make data frame with necessary information:
  df <- tibble(Tile.name = tilename, 
               Point.density = density, 
               Area.covered = area,
               Tile.max.X = extent_tile[1, 2],
               Tile.min.X = extent_tile[1, 1],
               Tile.max.Y = extent_tile[2, 2],
               Tile.min.Y = extent_tile[2, 1])
  
  return(df)
}

#' apply function to catalog:
catalog_stats <- catalog_apply(ctg_retiled, catalog_file_stats)
catalog_stats_combined <- 

ctg_retiled_polygons_joined <- ctg_retiled_polygons %>% 
  left_join(df2, by = "Tile.name")