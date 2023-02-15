library(sgsR)
library(terra)
library(mapview)
library(sf)
library(dplyr)



# 1 Import and Preprocess data -----------------------------------------------------------------------------------------


## The ALS raster data ----

#' load ALS raster representing the bioklim transects:
ALS_metrics_bioklim <- terra::rast("C:/Data/LiDAR_metrics_201608_10m.tiff")
plot(ALS_metrics_bioklim$BE_H_P10)

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


## The Bioklim reference plots ----

#' load the bioklim reference plots as points:
reference_points <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Inventory/Bioklim/LAI_TLS_plots_2017.gpkg")
mapview(reference_points)

#' change coordinate system for the reference points:
reference_points_transformed <- st_transform(reference_points, crs = crs(ALS_metrics_bioklim))


## The polygon base-maps of the BFNP ----

habitat_types <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Habitat_Types_Land_Cover.shp")

#' transform CRS:
habitat_types_transformed <- st_transform(habitat_types, crs = crs(ALS_metrics_bioklim))

#' filter habitat types
habitat_types_filtered <- habitat_types_transformed %>%
  filter(grepl('Nadel|Totholz|Laub|Misch|Latsche', KLS_DEU)) %>% 
  select(KLS_ENG)

#' clip the habitat types to the extent of the rasters
habitat_types_bioklim <- st_intersection(habitat_types_filtered, ALS_metrics_area_2)
mapview(habitat_types_bioklim)
habitat_types_bioklim


# 2 Stratification -----------------------------------------------------------------------------------------------------

## K-Means Classification ----

#' stratification based on k-means classification 10-strata: 
stratified_ALS <- strat_kmeans(mraster = ALS_metrics_bioklim, 
                               nStrata = 10, 
                               iter = 1000, 
                               algorithm = "MacQueen",
                               plot = T) 

stratified_habitat_types <- strat_poly(poly = habitat_types_bioklim, attribute = "KLS_ENG")

?strat_poly

## Perform principal component analysis ----
pcomp_ALS <- calculate_pcomp(mraster = ALS_metrics_bioklim,
                nComp = 3,
                plot = T)



# 3 Analysis -----------------------------------------------------------------------------------------------------------

## Calculate Representation ----

#' calculate the representation of the existing reference points based on the k-means classes:
calculate_representation(sraster = stratified_ALS,
                         existing = reference_points_transformed,
                         plot = T)



## Calculate sample size ----
calculate_sampsize(mraster = ALS_metrics_bioklim$BE_H_P10,
                        rse = 0.05,
                        start = 0.01,
                        end = 0.1,
                        increment = 0.01,
                        plot = T)

plot(pcomp_ALS$PC1)
x11();plot(ALS_metrics_bioklim$BE_H_P10)
