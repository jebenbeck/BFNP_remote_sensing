#' create a footprint polygon:

#' set all values != NA to 1
ALS_metrics_edit <- terra::ifel(ALS_metrics_bioklim$BE_H_P10 >= 0, 1, ALS_metrics_bioklim$BE_H_P10)
plot(ALS_metrics_edit)

#' convert raster to polygons to get extent
ALS_metrics_area <- st_as_sf(
  as.polygons(x = ALS_metrics_edit, dissolve = T,
              values = F, na.rm =T)
)
plot(ALS_metrics_area)