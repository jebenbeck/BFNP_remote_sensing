library(sf)
library(tidyverse)


Inventurpunkte_gesamt <- read_sf("G:/Waldstruktur Bodendaten/Waldinventur/Inventurpunkte Erweiterungsgebiet/Inventurpunkte_UTM32.shp") %>% 
  st_drop_geometry()

Inventurpunkte_2013 <- read_sf("G:/Waldstruktur Bodendaten/Waldinventur/Inventurpunkte Erweiterungsgebiet/Inventurpunkte_2013.shp") %>% 
  st_drop_geometry()


Inventurpunkte_neu <- full_join(Inventurpunkte_gesamt, Inventurpunkte_2013, by = "KOORD") %>% 
  mutate(X = coalesce(GAUSS_RW, X),
         Y = coalesce(GAUSS_HW, Y)
         ) %>% 
  select(c(KOORD, X, Y, letztmals, Anm)) %>% 
  rename(Anmerkung = Anm)

head(Inventurpunkte_neu)


sf_Inventurpunkte_neu <- st_as_sf(Inventurpunkte_neu, coords = c("X", "Y"), crs = 25832)

plot(sf_Inventurpunkte_neu)

st_write(sf_Inventurpunkt_neu, "G:/Waldstruktur Bodendaten/Waldinventur/Inventurpunkte Erweiterungsgebiet/Inventurpunkt_kombiniert.gpkg", driver = "GPKG")
