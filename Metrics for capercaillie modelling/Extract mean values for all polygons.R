library(RSDB)
library(raster)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)



#' load polygons:
poly <- st_read("H:/Daten Auerhuhn/GridsBeideBayerwald/quadrat50_gk6orig.shp")
str(poly)

#' load tree species distribution rasters:
tsd.rast <- rast("H:/Single tree polygons 2017/04_Cover Rasters_10m/Mosaic/Coverage_tree_types.tif")

#' extract mean value of raster pixels per area:
poly_val <- exact_extract(tsd.rast, poly, fun = "mean", progress = T)#na.rm = T

poly_val$mean.snag <- NULL
names(poly_val) <- c("Anteil_Laubwald", "Anteil_Nadelwald", "Anteil_Totholz")

#' combine the newly calculated columns with the polygons
poly_new <- cbind(poly, poly_val)
plot(poly_new)

poly_new
head(poly_val)