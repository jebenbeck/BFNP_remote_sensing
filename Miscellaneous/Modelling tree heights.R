## Info ----------------------------------------------------------------------------------------------------------------


#' Author: Jakob Ebenbeck
#' Last updated: 2023-12-10
#' Status: Early stage 


### Purpose of script ----

#' Modelling tree heights and stock based on stand height model curves per stand in the BFNP reference areas
 

### Notes ----


### Required datasets ----

#' A table containing the heights and diameters of trees

### Required packages ----

require(tidyverse)
require(pbapply)
require(sf)
require(mapview)
require(ggplot2)


### Required functions and scripts ----


### Set working directories ----



## 1. Calculate Height -------------------------------------------------------------------------------------------------


#' read in the data (table) containing heights and diameters of single trees 

Single_tree_data_raw <- st_read("F:/Waldstruktur Bodendaten/Dauerbeobachtungsflächen/HTO Referenzflächen/Datenaufnahme 2023/Baumdaten_Referenzflaechen.gpkg",
                                layer = "baumdaten_2023")
mapview(Single_tree_data_raw)


#' calculate the formulas per reference area:
edit <- Single_tree_data_raw %>% 
  filter(BHD > 7) %>% 
  group_by(Transekt) %>% 
  group_split()

ggplot(edit[[6]], aes(x = BHD, y = Hoehe, color = BA)) +
  geom_point() +
  geom_smooth(se = F, method = "gam", formula = y ~ log(x))

View(edit[[6]])

model <- lm(BHD~log(Hoehe),data=edit[[6]])
model

missing_rows <- is.na(edit[[6]]$Hoehe)

predicted_values <- predict(model, newdata = edit[[6]][missing_rows, ])
predicted_values

edit[[6]]$BHD[missing_rows] <- predicted_values

edit[[6]]



calc_bhk <- function(data, species, diameter, height)



## 2. Calculate stock --------------------------------------------------------------------------------------------------