Sys.setenv("PROJ_NETWORK"="ON")

library(sf)
library(mapview)
library(tidyverse)

#' check set up of sf package:
sf_proj_search_paths()
sf_extSoftVersion()
sf_proj_network(enable = T)
sf_proj_network()

#' get ground reference points data in GK and UTM:
reference_points <- read.csv("D:/Reproject ALS Data test/Transformation grids/KANU/Testpunkte_Echtumstellung.csv", sep = ";") %>% 
  rename("UTM32_X" = "E.UTM32",
         "UTM32_Y" = "N.UTM32.",
         "GK4_X" = "R.GK4.",
         "GK4_Y" = "H.GK4.")

rp_UTM <- st_as_sf(reference_points, coords = c("UTM32_X", "UTM32_Y"), crs = st_crs(25832), remove = F)
rp_GK <- st_as_sf(reference_points, coords = c("GK4_X", "GK4_Y"), crs = st_crs(31468), remove = F)

## Transformations -----------------------------------------------------------------------------------------------------


#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")

pipeline_BETA2007 <- options[1,]$definition


#' apply different transformations:

#' default:
rp_transformed_default <- st_transform(rp_GK, crs = 25832)
#' Beta2007 definition:
rp_transformed_BETA2007 <- st_transform(rp_GK, pipeline = pipeline_BETA2007)

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
accurracy_beta2007 <- calculate_accurracy(rp_transformed_default, "beta2007")

boxplot(accurracy_default)
boxplot(accurracy_beta2007)
