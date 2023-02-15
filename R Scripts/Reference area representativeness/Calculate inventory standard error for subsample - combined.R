library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(mapview)
library(terra)


# Load data ----


##' spatial data:

#' 200m inventory points:
Inventory_200m <- read_sf("C:/Users/Rieser/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Inventory/Inventory_points_200x200.gpkg", quiet = T) %>% 
  mutate(KOORD = as.character(round(KOORD, digits = 0)))

#' 800 m inventory points:
Inventory_800m <- read_sf("C:/Users/Rieser/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Inventory/Inventory_points_800x800.gpkg", quiet = T) %>% 
  mutate(KOORD = as.character(round(KOORD, digits = 0)))

#' National Park areas:
NP_Areas <- read_sf("C:/Users/Rieser/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/BFNP_Areas.gpkg")

##' Wood volume data of 200m inventory:

Inventory_200m_data <- read_excel(path = "C:/Users/Rieser/OneDrive/BFNP/Data/Other data/INVENTURBAUMART_IB.xlsx",
                                  sheet = 1,
                                  col_names = TRUE) %>% 
  mutate(INV_KREISKOORD = as.character(round(INV_KREISKOORD, digits = 0)))


# Preprocess the volume data table ----


Inventory_200m_volume_merged <- Inventory_200m_data %>% 
  filter(LAUFZEITBEGINN_FE == "2004") %>%               #' only use 2002 Inventory data
  group_by(INV_KREISKOORD) %>%                  #' calculate for each point per strata
  summarize(Volume = sum(VORRAT_HA)) %>%                #' calculate the volume
  select(c(INV_KREISKOORD, Volume)) %>%      #' select only relevant columns
  mutate(across(.cols = Volume,                      #' convert 0 to NA
                .fns = ~replace_na(., 0)))

Inventory_200m_volume_merged


# Join the different inventory points ----


#' filter the 200 inventory data to have unique points:
Inventory_200m_unique <- Inventory_200m %>% 
  filter(KOORD > 0) 

#' add the information on the NP Area to the inventory point IDs:
Inventory_200m_area <- st_intersection(NP_Areas, Inventory_200m_unique) %>%
  select(Text, KOORD) %>% 
  rename(Area = Text,
         Point_ID = KOORD)

Inventory_800m_edit <- Inventory_800m %>% 
  as.data.frame() %>% 
  select(KOORD) %>% 
  mutate("Subset_800m" = T) 

head(Inventory_800m_edit)

#' combine the inventory volume data and the spatial points
Inventory_data_final <- Inventory_200m_area %>% 
  left_join(y = Inventory_200m_volume_merged, by = c("Point_ID" = "INV_KREISKOORD")) %>% 
  #' join info on 800m plots:
  left_join(y = Inventory_800m_edit, by = c("Point_ID" = "KOORD"))

head(Inventory_data_final)



#' Metrics of the sample:
Metrics_sample <- Inventory_data_final %>% 
  as.data.frame() %>% 
  filter(Subset_800m == T) %>% 
  select(!geom) %>% 
  summarize(sum_volume = sum(Volume, na.rm = T),
            mean_volume = mean(Volume, na.rm = T),
            point_count = sum(!is.na(Volume)),
            var_volume = var(Volume, na.rm = T),
            sq_volume = sum(Volume*Volume, na.rm = T),
            var_sq_volume = (sq_volume-(sum_volume*sum_volume)/point_count)/point_count-1,
            var_mean_sq_volume = var_sq_volume/point_count,
            var_mean_volume = sqrt(var_mean_sq_volume)
  )

Metrics_sample


#' Metrics of the sample per area:
Metrics_sample_area <- Inventory_data_final %>% 
  as.data.frame() %>% 
  filter(Subset_800m == T) %>% 
  select(!geom) %>% 
  group_by(Area) %>%
  summarize(sum_volume = sum(Volume, na.rm = T),
            mean_volume = mean(Volume, na.rm = T),
            point_count = sum(!is.na(Volume)),
            sd_volume = var(Volume, na.rm = T),
            sq_volume = sum(Volume*Volume, na.rm = T),
            var_sq_volume = (sq_volume-(sum_volume*sum_volume)/point_count)/point_count-1,
            var_mean_sq_volume = var_sq_volume/point_count,
            var_mean_volume = sqrt(var_mean_sq_volume)
  )

Metrics_sample_area

#' calculate standard error for each metric and area:
Metrics_sample_area$SE <- Metrics_sample_area$var_mean_volume / Metrics_sample_area$mean_volume
Metrics_sample_area


write.csv(Metrics_sample_area, "C:/Users/Rieser/OneDrive/BFNP/Data/Metrics_sample_area_combined_volume.csv") 



#' Metrics sample Grundgesamtheit ----

#' calculate total mean volume:
total_mean_volume <- (1/5808)*(2699*382.947976 + 3109*325.752764)
total_mean_volume


# Total mean variance
total_mean_sq_variance <- (1/5808^2) * ( ((2699)^2*43702.07245)/173 + ((3109)^2*63083.84111)/199 )
total_mean_variance <- sqrt(total_mean_sq_variance)
total_mean_variance

#'coerce to df:
total_mean_var_volume <- data.frame(Gebiet = "Gesamtgebiet", 
                                    total_mean_volume = total_mean_volume,
                                    total_mean_variance =total_mean_variance)

total_mean_var_volume

#' calculate standard error:
total_mean_var_volume$SE <- total_mean_var_volume$total_mean_variance/total_mean_var_volume$total_mean_volume
total_mean_var_volume

write.csv(total_mean_var_volume, "C:/Users/Rieser/OneDrive/BFNP/Data/total_mean_var_combined_volume.csv") 


#' Determine sample size ----


#' full sample size regarding strata variance:
sample_size_full_coniferous <- (0.465*sqrt(47907.62657) + 0.535*sqrt(56134.64884))^2 / (total_mean_variance_coniferous^2)
sample_size_full_deciduous <- (0.465*sqrt(17392.35784) + 0.535*sqrt(15312.48778))^2 / (total_mean_variance_deciduous^2)

#' sample sizes:
sample_size_full_coniferous
sample_size_full_deciduous
