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
  mutate(Tree_type = recode(BAUMART,                      #' merge all tree species to the 2 strata coniferous & deciduous 
                            "Buche" = "Deciduous",
                            "Fichte" = "Coniferous",
                            "Tanne" = "Coniferous",
                            "Vogelbeere" = "Deciduous",
                            "Birke" = "Deciduous",
                            "Pappel" = "Deciduous",
                            "Bergahorn" = "Deciduous",
                            "Moorbirke" = "Deciduous",
                            "Spirke" = "Coniferous",
                            "Kiefer" = "Coniferous",
                            "Aspe" = "Deciduous",
                            "Weide" = "Deciduous",
                            "Laerche(europ)" = "Coniferous",
                            "Schwarzerle" = "Deciduous",
                            "Linde" = "Deciduous",
                            "Kirsche" = "Deciduous",
                            "Douglasie" = "Coniferous",
                            "Esche" = "Deciduous",
                            "Spitzahorn" = "Deciduous",
                            "Sommerlinde" = "Deciduous",
                            "Ulme" = "Deciduous",
                            "Eiche" = "Deciduous",
                            "Sonst. Laubholz" = "Deciduous",
                            "Edellaubholz" = "Deciduous",
                            "Roteiche" = "Deciduous",
                            "Eibe" = "Deciduous",
                            "Weißerle" = "Deciduous"),
  ) %>% 
  group_by(INV_KREISKOORD, Tree_type) %>%                  #' calculate for each point per strata
  summarize(Volume_HA = sum(VORRAT_HA)) %>%                #' calculate the volume
  select(c(INV_KREISKOORD, Tree_type, Volume_HA)) %>%      #' select only relevant columns
  pivot_wider(id_cols = INV_KREISKOORD,                    #' change the structure of the data frame to show the volume
              names_from = Tree_type,                      #' per point for both strata separately
              names_prefix = "Volume_",
              values_from = Volume_HA) %>%
  mutate(across(.cols = everything(),                      #' convert 0 to NA
                .fns = ~replace_na(., 0)))



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
plot(Inventory_data_final)


# Calculate metrics ----

#' Metrics of all 200m plots:
Metrics_full <- Inventory_data_final %>% 
  as.data.frame() %>% 
  select(!geom) %>% 
  pivot_longer(c(Volume_Coniferous, Volume_Deciduous), names_to = "Tree_type", values_to = "Volume") %>% 
  group_by(Tree_type) %>% 
  summarize(sum_volume = sum(Volume, na.rm = T),
            mean_volume = mean(Volume, na.rm = T),
            point_count = sum(!is.na(Volume)),
            var_volume = var(Volume, na.rm = T),
            sq_volume = sum(Volume*Volume, na.rm = T),
            var_sq_volume = (sq_volume-(sum_volume*sum_volume)/point_count)/point_count-1,
            var_mean_sq_volume = var_sq_volume/point_count,
            var_mean_volume = sqrt(var_mean_sq_volume)
  )

Metrics_full

#' Metrics of all 200m plots per area:
Metrics_full_area <- Inventory_data_final %>% 
  as.data.frame() %>% 
  select(!geom) %>% 
  pivot_longer(c(Volume_Coniferous, Volume_Deciduous), names_to = "Tree_type", values_to = "Volume") %>% 
  group_by(Area, Tree_type) %>% 
  summarize(sum_volume = sum(Volume, na.rm = T),
            point_count = sum(!is.na(Volume))
            ) %>% 
  ungroup() %>%
  group_by(Tree_type) %>% 
  mutate(proportion = point_count/sum(point_count))

Metrics_full_area


#' Metrics of the sample:
Metrics_sample <- Inventory_data_final %>% 
  as.data.frame() %>% 
  filter(Subset_800m == T) %>% 
  select(!geom) %>% 
  pivot_longer(c(Volume_Coniferous, Volume_Deciduous), names_to = "Tree_type", values_to = "Volume") %>% 
  group_by(Tree_type) %>%
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
  pivot_longer(c(Volume_Coniferous, Volume_Deciduous), names_to = "Tree_type", values_to = "Volume") %>% 
  group_by(Area, Tree_type) %>%
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

write.csv(Metrics_sample_area, "C:/Users/Rieser/OneDrive/BFNP/Data/Metrics_sample_area.csv") 


# Total volume per strata ----

#' 
#' #' Total volume per area:
#' total_mean_volume_coniferous_area <- data.frame(Area = c("RLG", "FRG"),
#'                                       Tree_type = "Coniferous",
#'                                       sum_volume = c(3109 * 229.922111, 2699 * 297.9011555),
#'                                       var_volume = c(3109 * 16.79534645, 2699 * 16.64099447)
#'                                       )
#' 
#' total_mean_volume_deciduous_area <- data.frame(Area = c("RLG", "FRG"),
#'                                      Tree_type = "Deciduous",
#'                                      sum_volume = c(3109*95.83065302, 2699*85.04682046),
#'                                      var_volume = c(3109*8.771953874, 2699*10.02665749)
#'                                      )
#' 
#' total_mean__var_volume_area <- rbind(total_mean_volume_coniferous_area, total_mean_volume_deciduous_area)
#' total_mean__var_volume_area$SE <- total_mean__var_volume_area$var_volume/total_mean__var_volume_area$sum_volume
#' total_mean__var_volume_area
#' #' -> redundant, same info as in metrics_sample_area 

#' Metrics sample Grundgesamtheit ----

#' calculate total mean volume:
total_mean_volume_coniferous <- (1/5808)*(2699*297.9011555 + 3109*229.922111)
total_mean_volume_coniferous
total_mean_volume_deciduous <- (1/5808)*(2699*85.04682046 + 3109*95.83065302)
total_mean_volume_deciduous


# Total mean variance
total_mean_sq_variance_coniferous <- (1/5808^2) * ( ((2699)^2*47907.62657)/173 + ((3109)^2*56134.64884)/199 )
total_mean_variance_coniferous <- sqrt(total_mean_sq_variance_coniferous)
total_mean_variance_coniferous

total_mean_sq_variance_deciduous <- (1/5808^2) * ( ((2699)^2*17392.35784)/173 + ((3109)^2*15312.48778)/199 )
total_mean_variance_deciduous <- sqrt(total_mean_sq_variance_deciduous)
total_mean_variance_deciduous

#'coerce to df:
total_mean_var_volume <- data.frame(Typ = c("Coniferous", "Deciduous"), 
                          total_mean_volume = c(total_mean_volume_coniferous, total_mean_volume_deciduous),
                          total_mean_variance = c(total_mean_variance_coniferous, total_mean_variance_deciduous)
                         )

#' calculate standard error:
total_mean_var_volume$SE <- total_mean_var_volume$total_mean_variance/total_mean_var_volume$total_mean_volume
total_mean_var_volume

write.csv(total_mean_var_volume, "C:/Users/Rieser/OneDrive/BFNP/Data/total_mean_var_volume.csv") 


#' Determine sample size ----


#' full sample size regarding strata variance:
sample_size_full_coniferous <- (0.465*sqrt(47907.62657) + 0.535*sqrt(56134.64884))^2 / (total_mean_variance_coniferous^2)
sample_size_full_deciduous <- (0.465*sqrt(17392.35784) + 0.535*sqrt(15312.48778))^2 / (total_mean_variance_deciduous^2)

#' sample sizes:
sample_size_full_coniferous
sample_size_full_deciduous
