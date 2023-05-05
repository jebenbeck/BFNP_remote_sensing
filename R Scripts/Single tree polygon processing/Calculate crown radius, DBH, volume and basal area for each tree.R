## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Rieser
#' Last updated: 2023
#' Status: Work in progress 


### Purpose of script ----


### Notes ----


### Required datasets ----


### Required packages ----

require(tidyverse)
require(parallel)
library(doParallel)
library(foreach)
require(sf)
require(terra)
require(dplyr)
require(raster)
require(mapview)
require(units)


### Required functions and scripts ----


### Set working directories ----

indir <- "F:/Single tree polygons 2017/Projected_UTM"
outdir <- "F:/Single tree polygons 2017/Volume/"




## 1. Processing -------------------------------------------------------------------------------------------------------


#' list of all polygons:
ST_files_list <- list.files(indir, pattern = ".*.gpkg$", recursive = T, full.names = TRUE)


#' set up parallel computation:
no_cores <- 8   #' number of cores
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)

#' apply processing chain for each file:
foreach(i = 1:length(ST_files_list), .packages = c("sf", "tidyverse", "units")) %dopar% {
#for(i in 1:length(ST_files_list_subset)) {

    #' select respective file
  ST_file <- ST_files_list[[i]]
  
  #' parse filename:
  filename <- basename(ST_file)
  
  #' read in the polygons:
  ST_polygons <- read_sf(ST_file, as_tibble = F, quiet = T) #%>% 
    #mutate(VALID = st_is_valid(.)) %>%              #' check individual polygons, if valid
    #filter(VALID == T)                              #' remove invalid polygons
  
  #' Process the data:
  ST_polygons_edit <- ST_polygons %>% 
    rename(
      TREE_ID = ID, 
      TREE_HEIGHT = HEIGHT,
      Z_POS = TER_HEIGHT,
      CROWN_BASE_HEIGHT = CB_HEIGHT,
      CROWN_VOLUME = CROWN_VOL) %>% 
    relocate(Z_POS, .after = Y_POS) %>% 
    relocate(c(TREE_HEIGHT, CROWN_BASE_HEIGHT), .after = TREE_CLASS) %>% 
    mutate(
      #' recode the tree classes:
      TREE_CLASS = recode(TREE_CLASS, 
                          'conif' = 'Coniferous', 
                          'decid' = 'Deciduous',
                          'deadw' = 'Dead',
                          'snag' = 'Snag'),
      
      #' calculate the crown area:
      CROWN_AREA = geom %>% st_area() %>% drop_units(),
      
      #' calculate the crown radius:
      CROWN_RADIUS = 2*sqrt(CROWN_AREA/pi),
      
      #' calculate the DBH:
      DBH = case_when(
        TREE_CLASS == "Coniferous" & Z_POS > 1100  ~ 8.2149 + -0.347989 * TREE_HEIGHT + 0.85412 * CROWN_RADIUS + 0.063952 * TREE_HEIGHT ^ 2 + 0.094879 * CROWN_RADIUS ^ 2,
        TREE_CLASS == "Coniferous" & Z_POS <= 1100  ~ exp (1.514295763 + 0.08884223 * TREE_HEIGHT + 0.02451053 * CROWN_RADIUS + -0.00081988 * TREE_HEIGHT ^ 2 + 0.00238753 * CROWN_RADIUS ^ 2),
        TREE_CLASS == "Deciduous" ~ exp (1.466743252 + 0.0896038 * TREE_HEIGHT + 0.08079226 * CROWN_RADIUS + -0.00108584 * TREE_HEIGHT ^ 2 + -0.0019256 * CROWN_RADIUS ^ 2)
      ),
    
      #' calculate the timber stock:
      STOCK = case_when(
        TREE_CLASS == "Coniferous" & Z_POS > 1100 ~ exp (-6.141586 + 0.33287 * TREE_HEIGHT + 0.271382 * CROWN_RADIUS + -0.004105 * TREE_HEIGHT ^ 2 + -0.0139 * CROWN_RADIUS ^ 2),
        TREE_CLASS == "Coniferous" & Z_POS <= 1100 ~ exp (-5.389 + 0.25144704 * TREE_HEIGHT + 0.06587771 * CROWN_RADIUS + -0.00232442 * TREE_HEIGHT ^ 2 + 0.00274139 * CROWN_RADIUS ^ 2),
        TREE_CLASS == "Deciduous" ~ exp (-6.1162059 + 0.29604055 * TREE_HEIGHT + 0.19163645 * CROWN_RADIUS + -0.00356909 * TREE_HEIGHT ^ 2 + -0.00523082 * CROWN_RADIUS ^ 2)
      ),
    
      #' calculate the basal area:
      BASAL_AREA = (DBH / 200) ^ 2 * pi,
      
      .after = CROWN_VOLUME) 
  
  #' export as gpkg:
  st_write(ST_polygons_edit, dsn = paste0(outdir, filename), driver = "GPKG", append = F)
  
  rm(ST_polygons)
  rm(ST_polygons_edit)

}

stopCluster(cl)



#' convert to excel



ST_gpkg_list <- list.files(outdir, pattern = ".*.gpkg$", recursive = T, full.names = TRUE)


#' Set up the parallel computation: 
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)

foreach(i = 1:length(ST_gpkg_list), .packages = c("sf", "writexl")) %dopar% {
  
  #' read polygons
  ST_file <- read_sf(ST_gpkg_list[[i]], quiet = T)
  
  #' set file name:
  filename <- tools::file_path_sans_ext(basename(ST_gpkg_list[[i]])) 
  
  #' export as xlsx:
  writexl::write_xlsx(ST_file, path = paste0(outdir, "Tables/", filename, ".xlsx"), col_names = T)
  
  rm(ST_file)
}

stopCluster(cl)



paste0(outdir, "Tables/", "test", ".xlsx")






