library(sf)
library(tidyverse)
library(ggplot2)
library(mapview)


## 1. Load and preprocess datasets -------------------------------------------------------------------------------------


#' load the correctly measured GPS-Points:
Inventory_plots_geolocation_GPS <- read_sf("F:/Waldstruktur Bodendaten/Waldinventur/Inventurpunkte Geodaten/Inventurpunkte_Geolocation.gpkg", layer = "GPS eingemessen") %>% 
  st_drop_geometry() %>% 
  mutate(KOORD = as.numeric(KOORD)) %>% 
  select(-c(GK_RECHTS, GK_HOCH, UTM33_X, UTM33_Y))

#' load the incorrectly measured Zeno-5 GPS-Points:
Inventory_plots_geolocation_Zeno5 <- read_sf("F:/Waldstruktur Bodendaten/Waldinventur/Inventurdaten Analyse Valentin/Zeno 5 verschoben/Zeno 5 verschoben.shp") %>% 
  st_drop_geometry() %>% 
  select(KOORD, LAGE_QLT, PDOP, Datum, Gerät, ETRS89_REC, ETRS89_HOC) %>% 
  rename(UTM32_X = ETRS89_REC,
         UTM32_Y = ETRS89_HOC)

#' Join the true coordinates to the false Zeno-5 ones :
Inventory_plots_combined <- left_join(Inventory_plots_geolocation_Zeno5, Inventory_plots_geolocation_GPS, by = "KOORD", suffix = c(".zeno", ".ref")) #%>% 
  #filter(UTM32_X.y > 0)
Inventory_plots_combined

Inventory_plots_combined



## 2. Check for relationships using linear regression ------------------------------------------------------------------


#' a data frame only holding observations with reference and zeno coordinates

Inventory_plots_combined_match <- Inventory_plots_combined %>% 
  filter(!is.na(UTM32_X.ref))

Inventory_plots_combined_match


### Visually analyze the data first ----


#' plot the X-Coordinates:
plot_X <- ggplot(data = Inventory_plots_combined, aes(x = UTM32_X.ref, y = UTM32_X.zeno)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Reference", y = "Zeno5") 

plot_X


#' plot the Y-Koordinates:
plot_Y <- ggplot(data = Inventory_plots_combined, aes(x = UTM32_Y.ref, y = UTM32_Y.zeno)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Reference", y = "Zeno5")

plot_Y


### Remove the outliers to have just good data:


#' make a linear model:
Model_X <- lm(UTM32_X.ref ~ UTM32_X.zeno, data = Inventory_plots_combined_match)
plot(residuals(Model_X))

#' add the residuals to the data frame:
Inventory_plots_combined_match$Residuals_X <- residuals(Model_X)
View(Inventory_plots_combined_match)

#' define a threshold (2 times standard deviation):
threshold <- 2 * sd(Inventory_plots_combined_match$Residuals_X)
threshold

#' add information to the table whether the threshold was exceeded:
Inventory_plots_combined_match$Residuals_X_exceeded <- abs(Inventory_plots_combined_match$Residuals_X) > threshold

#' Remove the values that have too high residuals
Inventory_plots_combined_match_good <- Inventory_plots_combined_match %>% 
  filter(Residuals_X_exceeded == FALSE)

Inventory_plots_combined_match_good


### Check for the relationships again ----


#' plot the X-Coordinates with the linear realtionship again:
plot_X <- ggplot(data = Inventory_plots_combined_match_good, aes(x = UTM32_X.ref, y = UTM32_X.zeno)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Reference", y = "Zeno5")

plot_X



## 3. Model the coordinates --------------------------------------------------------------------------------------------



### X-Coordinates ----


#' make a linear model:
Model_X <- lm(UTM32_X.ref ~ UTM32_X.zeno, data = Inventory_plots_combined_match_good)

# Predict the values to all data:
Inventory_plots_combined$UTM32_X.model <- predict(Model_X, newdata = data.frame(UTM32_X.zeno = Inventory_plots_combined$UTM32_X.zeno))


### Y-Coordinates ----

#' make a linear model:
Model_Y <- lm(UTM32_Y.ref ~ UTM32_Y.zeno, data = Inventory_plots_combined_match_good)

# Predict the values
Inventory_plots_combined$UTM32_Y.model <- predict(Model_Y, newdata = data.frame(UTM32_Y.zeno = Inventory_plots_combined$UTM32_Y.zeno))

Inventory_plots_combined


## 5. finalize the Zeno5 dataset and export it -------------------------------------------------------------------------


#' join the new coordinates to the original data:
Inventory_plots_geolocation_Zeno5 <- read_sf("F:/Waldstruktur Bodendaten/Waldinventur/Inventurdaten Analyse Valentin/Zeno 5 verschoben/Zeno 5 verschoben.shp") %>% 
  st_drop_geometry()

Inventory_plots_geolocation_Zeno5_modeled_Koord <- left_join(Inventory_plots_geolocation_Zeno5, 
                                                             Inventory_plots_combined %>% select(KOORD, UTM32_X.model, UTM32_Y.model),
                                                             by = "KOORD") %>% 
  select(-fid)
Inventory_plots_geolocation_Zeno5_modeled_Koord

sf_Zeno5_modeled <- st_as_sf(Inventory_plots_geolocation_Zeno5_modeled_Koord, coords = c("UTM32_X.model", "UTM32_Y.model"), crs = 25832)

mapview(sf_Zeno5_modeled)
head(sf_Zeno5_modeled)

st_write(sf_Zeno5_modeled, driver = "GPKG", 
         dsn = "F:/Waldstruktur Bodendaten/Waldinventur/Inventurdaten Analyse Valentin/Zeno 5 verschoben/Zeno 5 modelliert 2.gpkg", 
         append = F)

## 6. Join the new Zeno5 dataset to the GPS dataset --------------------------------------------------------------------


#' filter only the values that don't have any reference data:
Inventory_plots_combined_Zeno_only <- Inventory_plots_combined %>% 
  filter(is.na(UTM32_X.ref)) %>% 
  select(KOORD, PDOP.zeno, Datum.zeno, Gerät.Zeno, UTM32_X.model, UTM32_Y.model)



