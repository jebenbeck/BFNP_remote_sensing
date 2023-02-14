library(terra)
library(raster)

#' load in list of rasters:
raster_files <- list.files("D:/Einzelbaumpolygone 2017/Cover Rasters/", pattern = "*.tif$", recursive = T, full.names = TRUE)
filenames <- substr(basename(raster_files), 1 , nchar(basename(raster_files))-4)

#' load in rasters:
raster_list <- lapply(raster_files, rast)

#' reorder bands:
raster_list_ordered <- list()
for (i in 1:length(raster_list)) {
  raster_list_ordered[[i]] <- raster_list[[i]][[names(raster_list[[1]])]]
}

#' convert to a spatial raster collection:
raster_sprc <- sprc(raster_list_ordered)
raster_sprc

#' make mosaic from raster collection:
raster_mosaic <- mosaic(raster_sprc, fun = "sum")

#' set all values >1 to 1:
raster_mosaic$decid[raster_mosaic$decid>=1] <- 1
raster_mosaic$conif[raster_mosaic$conif>=1] <- 1
raster_mosaic$deadw[raster_mosaic$deadw>=1] <- 1
raster_mosaic$snag[raster_mosaic$snag>=1] <- 1
plot(raster_mosaic)
raster_mosaic

names(raster_mosaic) <- c("deciduous", "coniferous", "deadwood", "snag")

#' export tif files:
writeRaster(x = raster_mosaic, filename = "D:/Einzelbaumpolygone 2017/Cover Rasters/Tree_type_coverage_mosaic.tif", overwrite = T)



coverage_raster <- rast("D:/Single tree polygons 2017/Cover Rasters/Mosaic/Coverage_tree_types.tif")
coverage_raster
plot(coverage_raster)

#' change NA values to 0:
coverage_raster_0 <- subst(coverage_raster, NA, 0)
plot(coverage_raster_0)

#' make mask from reference raster:

#' read in data:
reference_image <- rast("D:/10m_reference_raster.tif") %>%
  project(y = crs(coverage_raster))

#' change value range to meet masking requirements:
reference_image[reference_image <= 0] <- NA
plot(reference_image)


#' mask the coverage raster:
coverage_raster_crop <- resample(coverage_raster_0, reference_image)
coverage_raster_crop

coverage_raster_masked <- mask(coverage_raster_crop, reference_image)
coverage_raster_masked
plot(coverage_raster_masked)
mapview(raster(coverage_raster_masked$deciduous))

terra::writeRaster(x = coverage_raster_masked, filename = "D:/Single tree polygons 2017/Cover Rasters/Mosaic/Coverage_tree_types.tif", overwrite = T)


