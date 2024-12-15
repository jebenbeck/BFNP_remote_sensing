library(RSDB)
library(raster)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)


#' load polygons:
poly <- st_read("F:/Daten Auerhuhn/GridsBeideBayerwald/quadrat50_gk6orig.shp")
poly




## 1. Mean single tree metrics -----------------------------------------------------------------------------------------


#' load tree species distribution rasters:
tsd.rast <- rast("F:/Single tree polygons 2017/04_Cover Rasters_10m/Mosaic/Coverage_tree_types.tif")

#' extract mean value of raster pixels per area:
poly_val <- exact_extract(tsd.rast, poly, fun = "mean", progress = T)#na.rm = T

poly_val$mean.snag <- NULL
names(poly_val) <- c("Anteil_Laubwald", "Anteil_Nadelwald", "Anteil_Totholz")

#' combine the newly calculated columns with the polygons
poly_new <- cbind(poly, poly_val)
plot(poly_new)

poly_new
head(poly_val)


## 2. Mean ALS metrics:


# ---- Connect to RSDB server ---- #

#' Provide the login-credentials in an local R file on your computer or via an object (format: "username:password")
source("C:/Users/Rieser/OneDrive/BFNP/Documents/RSDB/RSDB credentials.R")


#' Connect to the server
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 


## 2. Mean ALS metrics ------------------------------------------------------------------------------------------------


# ---- Get raster data ---- #

#' Select raster database:
ALS_metrics.db <- db$rasterdb("LiDAR_metrics_ALS_2017-06_10m")

#' define AOI (can be any spatial object)
#' in this case, it is the full extent of the data
AOI <- ALS_metrics.db$extent

#' query rasterstack of ALS Metrics: 
ALS_metrics.raster <- ALS_metrics.db$raster(ext = AOI, band = c(3, 5, 7, 8, 9, 11, 12, 13, 14))
plot(ALS_metrics.raster$BE_ELEV_MEAN)
crs(ALS_metrics.raster)


# ---- Postprocessing ---- #

#' convert to terra raster object (to preserve the layer names):
ALS_metrics.rast <- rast(ALS_metrics.raster)

names(ALS_metrics.rast)
names(ALS_metrics.rast) <- c("Meereshöhe", "Vegetationshöhe", "Vegetationsdichte_Canopy", "Vegetationsdichte_Regeneration", 
                             "Vegetationsdichte_Understory", "Vegetationsbedeckung_2m", "Vegetationsbedeckung_5m", "Vegetationsbedeckung_10m",
                             "Vertikale_Heterogenität")
names(ALS_metrics.rast)

#' export raster to disk:
terra::writeRaster(ALS_metrics.rast, "F:/Daten Auerhuhn/ALS_metrics_2017_10m.tif", overwrite = T)


#' extract mean value of raster pixels per area:
poly_val_2 <- exact_extract(ALS_metrics.rast, poly_new, fun = "mean", progress = T)
names(poly_val_2) <- names(ALS_metrics.rast)
names(poly_val_2)

#' combine the newly calculated columns with the polygons
poly_new_2 <- cbind(poly_new, poly_val_2)
poly_new_2
plot(poly_new_2, max.plot = 15)

#' export polygon file:
st_write(poly_new_2, "F:/Daten Auerhuhn/Result/Grid_metrics.gpkg")
