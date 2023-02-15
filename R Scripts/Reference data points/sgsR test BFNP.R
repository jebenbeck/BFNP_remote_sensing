## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Rieser
#' Last updated: 2023-02-15
#' Status: Finished


### Purpose of script ----

#' Testing the sgsR package functionalities in a real environment in the Bavarian Forest National Park
#' Therefore, the representation of various point and polygon datasets is tested based on a stratification based on ALS 
#' metrics and land cover maps for the full national park scale


### Required datasets ----

#' various sample plots and areas 
#' ALS metrics raster
#' Land cover polygons


### Required packages ----

require(sgsR)
require(terra)
require(mapview)
require(sf)
require(dplyr)



## 1. Import data ------------------------------------------------------------------------------------------------------


setwd("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park")


### The ALS metrics ----

ALS_metrics <- rast("F:/ALS_metrics_2017.tif")
names(ALS_metrics)


### The BFNP area ----

AOI_BFNP <- st_read("BFNP_Area_full.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics))


### The habitat types polygons ----

habitat_types <- st_read("Habitat_Types_Land_Cover.shp") %>% 
  st_transform(crs = crs(ALS_metrics))


### Several different reference points ----

#' load the bioklim inventory plots as points:
inventory_points_bioklim <- st_read("Transects Bioclim/Bioklim_points_2016.gpkg")

#' load the 800m inventory plots as points:
inventory_points_800m <- st_read("Inventory/Inventory_points_800x800.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics)) %>% 
  select("geom")

#' load the 45 biodiv reference points:
inventory_points_biodiv <- st_read("Transects Bioclim/Biodiv_points.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics))

#' load the 100 inventory points of Hooman:
inventory_points_100 <- st_read("Inventory/Inventory_points_100_2013.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics))



## 2. Preprocessing ----------------------------------------------------------------------------------------------------


### The habitat polygons ----

#' Filter, combine, rename and subset the habitat types
habitat_types_filtered <- habitat_types %>%
  filter(grepl('Nadel|Totholz|Laub|Misch|Latsche', KLS_DEU)) %>% 
  mutate(TYPE = recode(KLS_ENG,
                       "Coniferous stand - mature" = "Coniferous",
                       "Coniferous stand - medium" = "Coniferous",
                       "Coniferous stand - young" = "Coniferous",
                       "Dead wood - lying - coniferous regeneration" = "Coniferous",
                       "Dead wood - lying - deciduous regeneration" = "Deciduous",
                       "Dead wood - lying - mixed regeneration" = "Mixed",
                       "Deciduous stand - mature" = "Deciduous",
                       "Deciduous stand - medium" = "Deciduous",
                       "Deciduous stand - young" = "Deciduous",
                       "Mixed stand - young" = "Mixed",
                       "Mixed stand - mature" = "Mixed",
                       "Mixed stand - medium" = "Mixed",
                       "Scrub pine" = "Coniferous"
  )) %>% 
  select(TYPE) %>%
  filter(grepl("Coniferous|Deciduous|Mixed", TYPE))

#' clip to the extent of BFNP:
habitat_types_NPBW <- st_intersection(habitat_types_filtered, AOI_BFNP)

#' rasterize:
habitat_types_NPBW_raster <- terra::rasterize(habitat_types_NPBW, ALS_metrics, field = "TYPE") %>%
  catalyze()

#' rename the band
names(habitat_types_NPBW_raster) <- "strata"
unique(values(habitat_types_NPBW_raster))


## The ALS metrics ----

#' Mask the ALS metrics with the forest habitats as derived from the land cover map:
ALS_metrics_masked <- mask(ALS_metrics, habitat_types_NPBW_raster$strata)

#' subset to select only relevant metrics: 
ALS_metrics_masked <- subset(ALS_metrics_masked, c(2, 3, 4), negate = T)

#' rename the bands (metrics):
names(ALS_metrics_masked) <- c("AGB", "Canopy height Mean", "Canopy height SD", "Penetration rate canopy", 
                               "Penetration rate regeneration", "Penetration rate understory", "LAI",
                               "Vegetation cover 2m",  "Vegetation cover 5m",  "Vegetation cover 10m")



## 3. Stratification ---------------------------------------------------------------------------------------------------


### K-Means Classification based on ALS metrics ----

#' stratification based on k-means classification with 5-strata: 
stratified_ALS <- strat_kmeans(mraster = ALS_metrics_masked, 
                               nStrata = 3, 
                               iter = 1000, 
                               algorithm = "MacQueen",
                               plot = T) 
plot(stratified_ALS)


### Perform principal component analysis ----

#' PCA with 3 compontents
#' Just for testing, no further usage
pcomp_ALS <- calculate_pcomp(mraster = ALS_metrics_masked,
                nComp = 3,
                plot = F,
                details = T)
plot(pcomp_ALS$raster)



## 4. Calculate representation -----------------------------------------------------------------------------------------


### Representation based on ALS data ----

#' calculate the representation of the existing reference points based on the k-means classes of the ALS data

#' first, the 800m inventory points:
rep <- calculate_representation(sraster = stratified_ALS,
                         existing = inventory_points_800m,
                         plot = T)

#' second, the 100 plots of Hooman:
rep <- calculate_representation(sraster = stratified_ALS,
                         existing = inventory_points_100,
                         plot = T)

#' third, the 13X bioklim plots:
rep <- calculate_representation(sraster = stratified_ALS,
                         existing = inventory_points_bioklim,
                         plot = T)

#' last, the 45 biodiv plots:
rep <- calculate_representation(sraster = stratified_ALS,
                         existing = inventory_points_biodiv,
                         plot = T)


### Representation based on the habitat map ----

#' first, the 800m inventory points:
rep <- calculate_representation(sraster = habitat_types_NPBW_raster,
                                existing = inventory_points_800m,
                                plot = T)

#' second, the 100 plots of Hooman:
rep <- calculate_representation(sraster = habitat_types_NPBW_raster,
                                existing = inventory_points_100,
                                plot = T)

#' third, the 13X bioklim plots:
rep <- calculate_representation(sraster = habitat_types_NPBW_raster,
                                existing = inventory_points_bioklim,
                                plot = T)

#' last, the 45 biodiv plots:
rep <- calculate_representation(sraster = habitat_types_NPBW_raster,
                                existing = inventory_points_biodiv,
                                plot = T)



## 5. Calculate sample sizes -------------------------------------------------------------------------------------------


#' sample size based on the ALS metrics: 
ss_ALS_metrics <- calculate_sampsize(mraster = ALS_metrics_masked,
                        rse = 0.05,
                        start = 0.01,
                        end = 0.1,
                        increment = 0.01,
                        plot = T)
ss_ALS_metrics$plot

#' sample size based on the ALS stratification raster: 
ss_ALS_strat <- calculate_sampsize(mraster = stratified_ALS,
                        rse = 0.05,
                        start = 0.01,
                        end = 0.1,
                        increment = 0.01,
                        plot = T)
ss_ALS_strat$plot

#' sample size based on the habitat map:
ss_habitat <- calculate_sampsize(mraster = habitat_types_NPBW_raster,
                                     rse = 0.05,
                                     start = 0.01,
                                     end = 0.1,
                                     increment = 0.01,
                                     plot = T)
ss_habitat$plot



## 6. Sub-sampling -------------------------------------------------------------------------------------------------------


### Sub-sampling ----


#' sub-sampling based on the 800 m inventory points and a given sample size. Hereby, the most representative points are 
#' selected from the original dataset

#' first, extract the metrics from the ALS raster data:
inventory_points_metrics <- extract_metrics(mraster = ALS_metrics_masked, existing = inventory_points_800m)

#' as the subsampling is very time-intensive, check, if the file already exists to load it and skip the processing part 
if (file.exists("C:/Users/jakob/OneDrive/BFNP/Data/Other data/sgs/subsample_800_64.gpkg")){
  inventory_subsample <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Other data/sgs/subsample_800_64.gpkg")
} else {
  inventory_subsample <- sample_existing(existing = inventory_points_metrics, #' the existing sample
                                         nSamp = 150,                         #' the number of samples we want
                                         #' optional: include mraster metrics to guide sampling of existing:
                                         #raster = ALS_metrics_masked, 
                                         plot = TRUE,                         #' plot
                                         progress = T)                        #' add progress bar
  st_write(subsample, "C:/Users/jakob/OneDrive/BFNP/Data/Other data/sgs/subsample_800_64.gpkg", driver = "GPKG")
}

#' look at the data
mapview(inventory_subsample)


### Calculate representation for sub-sample ----

rep_sub <- calculate_representation(sraster = stratified_ALS,
                                existing = inventory_subsample,
                                plot = T)