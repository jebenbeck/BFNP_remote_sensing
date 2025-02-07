Sys.setenv("PROJ_NETWORK"="ON")

library(sf)
library(mapview)
library(tidyverse)

#' get ground reference points data in GK and UTM:
reference_points <- read.csv("D:/Reproject ALS Data test/Transformation grids/KANU/Testpunkte_Echtumstellung.csv", sep = ";") %>% 
  rename("UTM32_X" = "E.UTM32",
         "UTM32_Y" = "N.UTM32.",
         "GK4_X" = "R.GK4.",
         "GK4_Y" = "H.GK4.")

rp_GK <- st_as_sf(reference_points, coords = c("GK4_X", "GK4_Y"), crs = st_crs(31468), remove = F)

mapview(rp_GK)


## Transformations -----------------------------------------------------------------------------------------------------


### Without network connection (default bahavior) ----


#' check set up of sf package:
sf_proj_search_paths()
sf_extSoftVersion()
sf_proj_network() #' should be FALSE, if not run: sf_proj_network(enable = F)

#' run default transformation:
rp_transformed_default <- st_transform(rp_GK, crs = 25832)


### With network connection enabled ----


#' connect to proj library:
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
View(options)

#' run new default transformation:
rp_transformed_default_network <- st_transform(rp_GK, crs = 25832)

#' select BETA2007 transformation options:
pipeline_BETA2007 <- "+proj=pipeline +step +inv +proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +step +proj=hgridshift +grids=de_adv_BETA2007.tif +step +proj=utm +zone=32 +ellps=GRS80"

#' apply different transformations:
rp_transformed_BETA2007 <- st_transform(rp_GK, pipeline = pipeline_BETA2007)



## Calculate the accurracy -----------------------------------------------------------------------------------------------------


#' calculate the accurracy of the different transformations:
calculate_accurracy <- function(rp_transformed, transformation_name){
  rp_transformed %>% mutate(UTM32_X_transformed = st_coordinates(.)[,1],
                            UTM32_Y_transformed = st_coordinates(.)[,2],
                            X_difference = abs(UTM32_X - UTM32_X_transformed),
                            Y_difference = abs(UTM32_Y - UTM32_Y_transformed),
                            .before = geometry) %>% 
    st_drop_geometry() %>% 
    select(X_difference, Y_difference) %>% 
    rename_with(~ paste0(., "_", transformation_name))
  }

accurracy_default <- calculate_accurracy(rp_transformed_default, "default")
accurracy_default_network <- calculate_accurracy(rp_transformed_default_network, "default_network")

boxplot(accurracy_default)
boxplot(accurracy_default_network)
