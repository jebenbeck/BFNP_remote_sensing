devtools::install_github("https://github.com/tgoodbody/sgsR")
library(sgsR)

#--- Load mraster files ---#

r <- system.file("extdata", "mraster.tif", package = "sgsR")

#--- load the mraster using the terra package ---#

mraster <- terra::rast(r)
terra::plot(mraster)
mraster


#--- apply quantiles algorithm to mraster ---#

sraster <- strat_quantiles(mraster = mraster$zq90, # use mraster as input for stratification
                           nStrata = 8) # produce 8 strata
terra::plot(sraster)

#--- apply stratified sampling ---#

existing <- sample_strat(sraster = sraster, # use sraster as input for sampling
                         nSamp = 200, # request 200 samples
                         mindist = 100, # samples must be 100 m apart
                         plot = TRUE) # plot output

#--- random samples ---#

srs <- sample_srs(raster = sraster,
                  nSamp = 50)

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
calculate_sampsize(mraster = mraster,
                   rse = 0.01)


#--- change default threshold sequence values ---# 

#' if increment and rse are not divisible the closest value will be taken
p <- calculate_sampsize(mraster = mraster,
                        rse = 0.03,
                        start = 0.01,
                        end = 0.08,
                        increment = 0.01,
                        plot = T)
p
