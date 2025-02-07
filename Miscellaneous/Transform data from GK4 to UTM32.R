library(sf)
library(mapview)
library(tidyverse)
library(pbapply)


#' get ground reference points data in GK and UTM:
reference_points <- read.csv("D:/Reproject ALS Data test/Transformation grids/KANU/Testpunkte_Echtumstellung.csv", sep = ";") %>% 
  rename("UTM32_X" = "E.UTM32",
         "UTM32_Y" = "N.UTM32.",
         "GK4_X" = "R.GK4.",
         "GK4_Y" = "H.GK4.")

#' convert to sf object:
rp_GK <- st_as_sf(reference_points, coords = c("GK4_X", "GK4_Y"), crs = st_crs(31468), remove = F)

#' plot:
mapview(rp_GK)


## Transformations -----------------------------------------------------------------------------------------------------

#' connect to proj library:
sf_proj_network(enable = T)

#' get transformation options available with grid:
options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
options[1,]

#' run new default transformation:
rp_transformed <- st_transform(rp_GK, crs = 25832)
rp_transformed


## Calculate the accurracy -----------------------------------------------------------------------------------------------------


#' calculate the accurracy of the transformation:
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

accurracy_default <- calculate_accurracy(rp_transformed, "default")

#' plot the accuracy:
boxplot(accurracy_default)



## Parallel processing ----------------------------------------------------------------------------------------------------------


#' make list to apply parallel:
file_list <- replicate(10, rp_GK, simplify = FALSE)

#' Use pbapply for processing files in parallel:

reproject_files <- function(file){
  
  #' transform las data:
  file_transformed <- sf::st_transform(file, crs = 25832)
  
  #' export the LAS object to a LAS file
  output_file <- file_transformed %>% mutate(UTM32_X_transformed = st_coordinates(.)[,1],
                                           UTM32_Y_transformed = st_coordinates(.)[,2],
                                           X_difference = abs(UTM32_X - UTM32_X_transformed),
                                           Y_difference = abs(UTM32_Y - UTM32_Y_transformed),
                                           .before = geometry) %>% 
    st_drop_geometry() %>% 
    select(X_difference, Y_difference)
  
  return(output_file)
}


cluster <- parallel::makeCluster(4)

#parallel::clusterExport(cluster)
parallel::clusterEvalQ(cluster, library(tidyverse)) # Load lidR on all nodes
parallel::clusterEvalQ(cluster, library(sf)) # Load tidyverse on all nodes

output_files <- pblapply(file_list, reproject_files, cl = cluster)

parallel::stopCluster()


boxplot(output_files[[5]])
