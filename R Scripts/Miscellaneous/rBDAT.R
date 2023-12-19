require(rBDAT)
require(sf)
require(tidyverse)

trees <- st_read("F:/Waldstruktur Bodendaten/Dauerbeobachtungsflächen/HTO Referenzflächen/Datenaufnahme 2023/HTO_Baumdaten_20231026.gpkg") %>% 
  as.data.frame()

trees_SH <- trees %>% 
  filter(baumart == "Fichte",
         stehend == "ja",
         bhd > 7,
         hoehe > 0,
         gipfelbruch == "nein"
         ) %>% 
  select(c(id, baumart, bhd, hoehe)) %>% 
  mutate("spp" = getSpeciesCode(baumart),
         "Dx" = bhd
         ) %>% 
  arrange(desc(bhd))

head(trees_SH)
nrow(trees_SH)

test <- getHeight(trees_SH)




## example for height calculation
tree <- list(spp = c(1, 1), D1 = c(30, 25), H = c(25, 30))
vars <- list(Dx = c(30, 25))
res <- buildTree(tree = tree, check = "height", vars = vars)
head(res)
class(res)