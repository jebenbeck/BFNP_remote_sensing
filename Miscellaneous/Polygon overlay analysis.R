library(sf)
library(terra)
library(dplyr)

reference_areas_2002 <- read_sf("C:/Users/jakob/OneDrive/BFNP/Data/Reference_Areas_combined.gpkg")
plot(reference_areas_2002)
names(reference_areas_2002)

reference_areas_2013 <- read_sf("D:/WIP/Referenzflächen/Referenzflächen_2/Referenzflächen.shp") %>% 
  st_transform(crs = crs(reference_areas_2002))

reference_areas_2013$RECORD_2016 <- T
reference_areas_2013

plot(reference_areas_2013)


reference_areas_2002_new <- st_join(reference_areas_2002, reference_areas_2013, join = st_covered_by) 
names(reference_areas_2002_new)
nrow(reference_areas_2002)
View(reference_areas_2002_new)

st_write(reference_areas_2002_new, "C:/Users/jakob/OneDrive/BFNP/Data/Reference_Areas_combined_new.gpkg")