require(rBDAT)
require(sf)
require(tidyverse)

trees <- st_read("G:/Stammfußkart_ArcGIS_new/2017_BHD_Puffer.shp") %>% 
  as.data.frame()

trees_count <- trees %>% 
  group_by(Transektnu) %>% 
  count(Objekt == "Fichte")
print(trees_count, n=50)


trees_SH <- trees %>% 
  filter(Transektnu == "53_Sulzschachten_b",
         Objekt == "Fichte",
         Brusthoehe >=0,
         Gesamthoeh >=0
         ) %>% 
  select(c(ID, Objekt, Brusthoehe, Gesamthoeh)) %>% 
  mutate("spp" = getSpeciesCode(Objekt),
         "Dx" = Brusthoehe
         )

head(trees_SH)
nrow(trees_SH)

test <- getHeight(trees_SH)

tree <- data.frame(spp = c(1, 1), D1 = c(30, 25), H = c(25, 20), Dx = c(7, 7))
tree
getHeight(tree, bark = TRUE)
