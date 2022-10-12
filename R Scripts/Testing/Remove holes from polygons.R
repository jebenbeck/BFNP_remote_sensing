library(sf)
install.packages("nngeo")
library(nngeo)

NPBW_area <- st_read("C:/Users/jakob/ownCloud/Data/Basisdaten/NP Bayerischer Wald/NPBW_Area_full.gpkg")
mapview::mapview(NPBW_area)

NPBW_area_smoothed <- st_remove_holes(NPBW_area)
mapview::mapview(NPBW_area) + mapview::mapview(NPBW_area_smoothed)

st_write(NPBW_area_smoothed, dsn = "C:/Users/jakob/ownCloud/Data/Basisdaten/NP Bayerischer Wald/NPBW_Area_full_smoothed.gpkg", driver = "GPKG")
?st_write
