library(sgsR)
library(terra)
library(mapview)
library(sf)
library(dplyr)



# 1 Import data --------------------------------------------------------------------------------------------------------


## The ALS metrics ----

ALS_metrics <- rast("F:/ALS_metrics_2017.tif")
names(ALS_metrics)


## The BFNP area ----

AOI_BFNP <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/BFNP_Area_full.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics))


## The habitat types polygons ----

habitat_types <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Habitat_Types_Land_Cover.shp") %>% 
  st_transform(crs = crs(ALS_metrics))


## The Reference points ----

#' load the bioklim inventory plots as points:
inventory_points_bioklim <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Transects Bioclim/Bioklim_points_2016.gpkg")

#' load the 800m inventory plots as points:
inventory_points_800m <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Inventory/Inventory_points_800x800.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics)) %>% 
  select("geom")

#' load the 45 biodiv reference points:
inventory_points_biodiv <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Transects Bioclim/Biodiv_points.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics))

#' load the 100 inventory points of Hooman:
inventory_points_100 <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Inventory/Inventory_points_100_2013.gpkg") %>% 
  st_transform(crs = crs(ALS_metrics))



# 2 Preprocess data --------------------------------------------------------------------------------------------------------


## The habitat polygons ----

#' Filter, combine and subset the habitat types
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

#' clip the habitat types to the extent of BFNP:
habitat_types_NPBW <- st_intersection(habitat_types_filtered, AOI_BFNP)

#' rasterize the habitat polygons:
habitat_types_NPBW_raster <- terra::rasterize(habitat_types_NPBW, ALS_metrics, field = "TYPE") %>%
  catalyze()

names(habitat_types_NPBW_raster) <- "strata"
unique(values(habitat_types_NPBW_raster))


## The ALS metrics ----

#' Mask and subset the ALS raster stack
ALS_metrics_masked <- mask(ALS_metrics, habitat_types_NPBW_raster$strata)
ALS_metrics_masked <- subset(ALS_metrics_masked, c(2, 3, 4), negate = T) #' select only relevant metrics
names(ALS_metrics_masked) <- c("AGB", "Canopy height Mean", "Canopy height SD", "Penetration rate canopy", 
                               "Penetration rate regeneration", "Penetration rate understory", "LAI",
                               "Vegetation cover 2m",  "Vegetation cover 5m",  "Vegetation cover 10m")
plot(ALS_metrics_masked$AGB)



# 2 Stratification -----------------------------------------------------------------------------------------------------


## K-Means Classification ----

#' stratification based on k-means classification 5-strata: 
stratified_ALS <- strat_kmeans(mraster = ALS_metrics_masked, 
                               nStrata = 3, 
                               iter = 1000, 
                               algorithm = "MacQueen",
                               plot = T) 
plot(stratified_ALS)

## Perform principal component analysis ----

pcomp_ALS <- calculate_pcomp(mraster = ALS_metrics_masked,
                nComp = 3,
                plot = F,
                details = T)
plot(pcomp_ALS$raster)



# 3 Representation -----------------------------------------------------------------------------------------------------------


## Representation based on ALS data ----

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


## Representation based on the habitat map ----

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


# 4 Calculate sample sizes -----------------------------------------------------------------------------------------------


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





# 5 Sampling -------------------------------------------------------------------------------------------------------


## Sub-sampling ----


#' sub-sampling based on the 800 m inventory points and a given sample size:

#' first, extract the metrics from the ALS raster data:
inventory_points_metrics <- extract_metrics(mraster = ALS_metrics_masked, existing = inventory_points_800m)

#' as the subsampling is very time-intensive, check, if the file already exists to load it and skip the processing part 
if (file.exists("C:/Users/jakob/OneDrive/BFNP/Data/Other data/sgs/subsample_800_64.gpkg")){
  inventory_subsample <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Other data/sgs/subsample_800_64.gpkg")
} else {
  inventory_subsample <- sample_existing(existing = inventory_points_metrics, # our existing sample
                                         nSamp = 150, # the number of samples we want
                                         #raster = ALS_metrics_masked, # include mraster metrics to guide sampling of existing
                                         plot = TRUE, # plot
                                         progress = T) #progress bar
  st_write(subsample, "C:/Users/jakob/OneDrive/BFNP/Data/Other data/sgs/subsample_800_64.gpkg", driver = "GPKG")
}

# Calculate representation again for subsample:
rep_sub <- calculate_representation(sraster = stratified_ALS,
                                existing = inventory_subsample,
                                plot = T)
mapview(inventory_subsample)
plot(ALS_metrics_masked$`Penetration rate canopy`)


## Stratify sample points ----

#' first, extract the metrics from the ALS raster data:
inventory_points_metrics <- extract_metrics(mraster = ALS_metrics_masked, existing = inventory_points_800m)


