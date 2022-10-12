library(sgsR)
library(terra)
library(mapview)
library(raster)

S2_median_metrics <- terra::rast("C:/Users/jakob/OneDrive/Desktop/median_Sentinel-2_BFNP_2017-01-01_2021-12-31.tif")

#plot(S2_median_metrics)
S2_median_metrics


#--- apply quantiles algorithm to mraster ---#
sraster <- strat_quantiles(mraster = S2_median_metrics$NDVI_median, # use mraster as input for stratification
                           nStrata = 8) # produce 4 strata

rraster <- raster::raster(sraster)
mapview::mapView(rraster)

#--- apply stratified sampling ---#

existing <- sample_strat(sraster = sraster, # use sraster as input for sampling
                         nSamp = 200, # request 200 samples
                         mindist = 100, # samples must be 100 m apart
                         plot = TRUE) # plot output

srs <- sample_srs(raster = sraster,
                  nSamp = 50)

mapview(srs)


#--- calculate representation ---#

#' calculates the representation of the existing samples based on the input rasters

#' of the random samples:
calculate_representation(sraster = sraster,
                         existing = srs,
                         plot = T)

#' of the stratified samples:
calculate_representation(sraster = sraster,
                         existing = existing,
                         plot = TRUE)

#--- determine sample size based on relative standard error (rse) ---#

#' of 1% :
calculate_sampsize(mraster = S2_median_metrics$NDVI_median,
                   rse = 0.01)


#--- change default threshold sequence values ---# 

#' if increment and rse are not divisible the closest value will be taken
p <- calculate_sampsize(mraster = S2_median_metrics$NDVI_median,
                        rse = 0.03,
                        start = 0.01,
                        end = 0.08,
                        increment = 0.01,
                        plot = T)
p
