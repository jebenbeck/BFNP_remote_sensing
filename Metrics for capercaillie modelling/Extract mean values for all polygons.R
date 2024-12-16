library(RSDB)
library(raster)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(mapview)
library(pbapply)

setwd("F:/Daten Auerhuhn/")

#' load polygons:
poly <- st_read("Ausgangsdaten/Grid/GridsBeideBayerwald/quadrat50_gk6orig.shp")
poly$X <- NULL
poly$Y <- NULL

mapview(poly)



## 1. Calculate species distribution -----------------------------------------------------------------------------------

#' load masks:
mask_npbw <- st_read("Ausgangsdaten/NPBW_polygon/Nationalpark_aussengrenze_vor2018.shp")
mask_npbw$ID <- NULL

mask_nps <- st_read("Ausgangsdaten/NPS_polygon/NPS_polygon.shp")
mask_nps$META_ID <- NULL
mask_nps$TEXT <- NULL

#' buffer:
mask_nps_buffered <-  st_buffer(mask_nps, 30)
mask_npbw_buffered <-  st_buffer(mask_npbw, 30)

#' combine mask data:
mask_full <- st_union(mask_npbw_buffered, mask_nps_buffered, by_feature = F)

#' fill holes:
mask_final <- nngeo::st_remove_holes(mask_full)

mapview(mask_final)

#' load tree species distribution rasters:
tsd.rast <- rast("Ausgangsdaten/Baumartenverteilung/TSD_2017_10m.tif")

#' mask to fit area:
tsd_masked.rast <- mask(tsd.rast, mask = mask_final)

#' export:
writeRaster(tsd_masked.rast, "Ausgangsdaten/Baumartenverteilung/TSD_2017_10m_masked.tif", overwrite =T)

#' extract mean value of raster pixels per area:
poly_val <- exact_extract(tsd_masked.rast, poly, fun = "mean", progress = T)

poly_val$mean.snag <- NULL
names(poly_val) <- c("Anteil_Laubwald", "Anteil_Nadelwald", "Anteil_Totholz")

# Calculate percentage of NA cells under each polygon
percent_NA <- exact_extract(tsd_masked.rast$deciduous, poly, function(values, coverage_fraction) {
  total_cells <- sum(coverage_fraction)
  na_cells <- sum(coverage_fraction[is.na(values)])
  return(100 * na_cells / total_cells)
}, progress = T)

#' Replace NA results with 0
percent_NA[is.na(percent_NA)] <- 0

#' Add the NA percentage as a new attribute to your polygons
poly_val$Anteil_NA <- percent_NA

#' combine the newly calculated columns with the polygons
poly_tsd <- cbind(poly, poly_val)
head(poly_tsd)


#' export polygon file:
st_write(poly_tsd, "Ergebnisse/Grid_Baumartenverteilung.gpkg", append = F)



## 2. Calculate ALS metrics --------------------------------------------------------------------------------------------


# ---- Connect to RSDB server ---- #

#' Provide the login-credentials in an local R file on your computer or via an object (format: "username:password")
source("C:/Users/Rieser/OneDrive/BFNP/Documents/RSDB/RSDB credentials.R")


#' Connect to the server
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 


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

#' mask the "Vegetationshöhe" band with another one to change 0s to NA:
plot(ALS_metrics.rast$Vegetationshöhe)
ALS_metrics.rast$Vegetationshöhe <- mask(ALS_metrics.rast$Vegetationshöhe, ALS_metrics.rast$Meereshöhe)
plot(ALS_metrics.rast$Vegetationshöhe)

#' export raster to disk:
terra::writeRaster(ALS_metrics.rast, "Ausgangsdaten/ALS_Metriken/ALS_metrics_2017_10m.tif", overwrite = T)

ALS_metrics.rast <- rast("Ausgangsdaten/ALS_Metriken/ALS_metrics_2017_10m.tif")

#' extract mean value of raster pixels per area:
poly_val_2 <- exact_extract(ALS_metrics.rast, poly, fun = "mean", progress = T)

names(poly_val_2) <- names(ALS_metrics.rast)
names(poly_val_2)

# Calculate percentage of NA cells under each polygon
percent_NA <- exact_extract(ALS_metrics.rast$Meereshöhe, poly, function(values, coverage_fraction) {
  total_cells <- sum(coverage_fraction)
  na_cells <- sum(coverage_fraction[is.na(values)])
  return(100 * na_cells / total_cells)
}, progress = T)


#' Replace NA results with 0
percent_NA[is.na(percent_NA)] <- 0

#' Add the NA percentage as a new attribute to your polygons
poly_val_2$Anteil_NA <- percent_NA

#' combine the newly calculated columns with the polygons
poly_new_2 <- cbind(poly, poly_val_2)
poly_new_2

#' export polygon file:
st_write(poly_new_2, "Ergebnisse/Grid_Metriken_ALS.gpkg", append = F)



## 3. Calculate CLC proportional coverage ------------------------------------------------------------------------------


# Load raster
CLC.rast <- rast("F:/Daten Auerhuhn/Ausgangsdaten/CLC raster/DATA/U2018_CLC2018_V2020_20u1.tif") 

#' transform polygons to fit raster crs:
poly_2 <- st_transform(poly, crs = crs(CLC.rast))

#' clip CLC raster to polygon extent:
CLC_clip.rast <- crop(CLC.rast, ext(poly_2))
plot(CLC_clip.rast)

calc_proportion_value <- function(value) {
  #' filter single value:
  CLC_filtered.rast <- CLC_clip.rast == value
  #' Extract proportions for raster values:
  results <- exact_extract(CLC_filtered.rast, poly, function(values, coverage_fraction) {
    sum(values * coverage_fraction) / sum(coverage_fraction)
  })
  return(results)
}

#' Define the range of values to calculate
values <- c(23:26, 29)

#' Apply the function to each value using pbapply
results_list <- pblapply(values, calc_proportion_value)

#' Combine the results into a data frame
results_df <- as.data.frame(do.call(cbind, results_list))

#' Name the columns with the corresponding values
colnames(results_df) <- c("Anteil_CLC311_Laubwald", "Anteil_CLC312_Nadelwald", "Anteil_CLC313_Mischwald", 
                          "Anteil_CLC321_Grasland", "Anteil_CLC324_Strauch")

#' View the resulting data frame
head(results_df)

#' combine the newly calculated columns with the polygons
poly_new_3 <- cbind(poly, results_df)

#' export polygon file:
st_write(poly_new_3, "Ergebnisse/Grid_Anteile_CLC.gpkg", append = F)

