library(sf)
library(tidyverse)
library(mapview)

## 1. Daten 2013 -------------------------------------------------------------------------------------------------------


### Neu ----


data_2013 <- st_read("E:/Waldstruktur Bodendaten/HTO Referenzflächen/Originaldaten/Daten 2013/Gesamttabelle_Objekte_bearbeitet.gpkg")
head(data_2013)

data_2013_transformed <- data_2013 %>% 
  #' select onle necessary columns:
  select(c(-field_21, -field_22, -field_23, -Aufnahmejahr)) %>% 
  #' rename the columns:
  rename(ID_Transekt = Transektnummer,
         ID_Objekt = ID,
         BHD = BHD..m.R.2013,
         Abgestorben = abgestorben.2013,
         Liegend = liegend.2013,
         Anfangsdurchmesser = Anfangsdurchmesser_mit_Rinde,
         Enddurchmesser = Enddurchmesser_mit_Rinde,
         Kronenansatz = Kronenansatz.2013,
         Höhe = Gesamthöhe.2013,
         Länge = Laenge,
         Bemerkung = Bemerkung.2013,
         Höhe_NN = Hoehe_NN_1,
         X_GK = GK_rechts_1,
         Y_GK = GK_hoch_1) %>% 
  #' convert the contents of some of the attributes:
  mutate(
  Liegend = case_when(
    Liegend == "x" ~ TRUE,
    is.na(Liegend) ~ FALSE),
  außerhalb_Transekt = case_when(
    außerhalb_Transekt == "x" ~ TRUE,
    außerhalb_Transekt == "+" ~ TRUE,
    außerhalb_Transekt == "außerhalb" ~ TRUE,
    is.na(außerhalb_Transekt) ~ FALSE),
  Abgestorben = case_when(
    Abgestorben == "x" ~ TRUE,
    is.na(Abgestorben) ~ FALSE),
  Objekt = case_when(
    Objekt == "Bu" ~ "Buche",
    Objekt == "Vobe" ~ "Vogelbeere",
    Objekt == "Fi" ~ "Fichte",
    Objekt == "Ta" ~ "Tanne",
    Objekt == "NH" ~ "Nadelholz",
    Objekt == "Sah" ~ "Spitzahorn",
    Objekt == "Bah" ~ "Bergahorn",
    Objekt == "Wute" ~ "Wurzelteller",
    TRUE ~ Objekt),
 # Baum = case_when(
#    Objekt %in% c("Buche", "Fichte", "Tanne", "Vogelbeere", "Nadelholz", "Spitzahorn", "Bergahorn", "Baum", "Totholz", "Ahorn")  ~ TRUE), .after = Objekt
  )

st_write(data_2013_transformed, "D:/Waldstruktur Bodendaten/HTO Referenzflächen/Originaldaten/Daten 2013/Gesamttabelle_Objekte_aufbereitet.gpkg", driver = "GPKG", overwrite =T)


#' Keeping only standing, existing trees in 2013:

trees_2013_standing_existing_BHD <- data_2013_transformed %>% 
  filter(Objekt %in% c("Buche", "Fichte", "Tanne", "Vogelbeere", "Nadelholz", "Spitzahorn", "Bergahorn", "Baum", "Totholz", "Ahorn"),
         Liegend != "TRUE",
         is.na(Anfangsdurchmesser),
         !is.na(BHD),
         #' filter empty geometries (for now):
         !st_is_empty(geom)
         ) %>% 
  select(c(-Anfangsdurchmesser, -Enddurchmesser, -Liegend))

head(trees_2013_standing_existing_BHD)
st_write(trees_2013_standing_existing_BHD, "D:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_2013_GK.gpkg", 
         driver = "GPKG", append = F)




## 2. Daten 2017 -------------------------------------------------------------------------------------------------------




data_2017 <- st_read("E:/Waldstruktur Bodendaten/HTO Referenzflächen/Originaldaten/Daten 2017/HTO 2017.gpkg")
unique(data_2017$Objekt)

data_2017_transformed <- data_2017 %>% 
  #' select onle necessary columns:
  select(c(-VOL, -field_1, -project, -year, -Lage.Qualitaet.cm)) %>% 
  #' separate the ID:
  mutate(ID_Transekt = as.integer(substr(plot, 1, 2)),
         Transektabschnitt = NA,
         Probequadrat = NA,
         Objektnummer = NA,
         .after = plot) %>% 
  select(-plot) %>% 
  rename(ID_Objekt = ID,
         Abgestorben = l.t,
         Liegend = l.s,
         Kronenansatz = Kronenans,
         Höhe = Hoehe,
         X_GK = GK_RW,
         Y_GK = GK_HW) %>% 
  #' convert the contents of some of the attributes:
  mutate(
    Liegend = case_when(
      Liegend == "liegend" ~ TRUE,
      Liegend == "stehend" ~ FALSE),
    Abgestorben = case_when(
      Abgestorben == "tot" ~ TRUE,
      Abgestorben == "lebend" ~ FALSE),
    Objekt = case_when(
      Objekt == "Bu" ~ "Buche",
      Objekt == "Vobe" ~ "Vogelbeere",
      Objekt == "Fi" ~ "Fichte",
      Objekt == "Ta" ~ "Tanne",
      Objekt == "NH" ~ "Nadelholz",
      Objekt == "Sah" ~ "Spitzahorn",
      Objekt == "Bah" ~ "Bergahorn",
      Objekt == "NA" ~ NA)
    )

trees_2017_standing_existing_BHD <- data_2017_transformed %>% 
  filter(Liegend != "TRUE",
         BHD != "NA"
  ) %>% 
  select(-Liegend)

st_write(trees_2017_standing_existing_BHD, "D:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_2017_GK.gpkg", 
         driver = "GPKG", append = F)




#' figure out the IDs of the 2017 data:
trees_2017_standing_existing_BHD$ID_Objekt
trees_2013_standing_existing_BHD$ID_Objekt



## 4. Merging the data -------------------------------------------------------------------------------------------------

#' as in the 2013 data some of the coordinates are mixed up, we need to merge the ones from 2017, as they are appeently correct.
#' for now, to get the most recent data, it's enough to 
#' This also means, that we have to make a jointly formatted ID

list_trees_ID <- trees_2017_standing_existing_BHD %>% 
  mutate(Länge_ID = nchar(ID_Objekt)) %>% 
  group_split(Länge_ID)


list_trees_ID_1 <- list_trees_ID[[1]]
list_trees_ID_2 <- list_trees_ID[[2]]

list_trees_ID_1_edit <- list_trees_ID_1 %>%
  mutate(Objektnummer_1 = substr(ID_Objekt, 2,9),
         Objektnummer = str_replace(Objektnummer_1, "^0+", "")
         ) %>% 
  select(-Objektnummer_1, -Länge_ID)

list_trees_ID_2_edit <- list_trees_ID_2 %>% 
  separate(ID_Objekt, into = c("ID_Transekt", "Transektabschnitt", "Objektnummer_1"), sep =" ", remove = F) %>% 
  mutate(Probequadrat = substr(Objektnummer_1, 1, 1),
         Objektnummer_2 = substr(Objektnummer_1, 2, 9),
         Objektnummer = str_replace(Objektnummer_2, "^0+", "")
  ) %>% 
  select(-Objektnummer_1, -Objektnummer_2, -Länge_ID)

trees_2017_new_ID_1 <- rbind(list_trees_ID_2_edit, list_trees_ID_1_edit)

selected_cols <- c("Transektabschnitt", "Probequadrat")

trees_2017_new_ID <- trees_2017_new_ID_1 %>% 
  mutate(across(all_of(selected_cols), ~ ifelse(is.na(.), " ", .))) %>% 
  unite(ID_Objekt_neu, c(ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer), sep = "/", na.rm = T, remove = F) %>% 
  select(ID_Objekt_neu, ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer, Objekt, Abgestorben, Kronenansatz, BHD, Höhe, Bemerkung, X_GK, Y_GK) %>% 
  st_drop_geometry()
  
head(trees_2017_new_ID)



trees_2013_new_ID <- trees_2013_standing_existing_BHD %>% 
  mutate(across(all_of(selected_cols), ~ ifelse(is.na(.), " ", .))) %>% 
  unite(ID_Objekt_neu, c(ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer), sep = "/", na.rm = T, remove = F) %>% 
  select(ID_Objekt_neu, ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer, außerhalb_Transekt, Objekt, Abgestorben, Kronenansatz, BHD, Höhe, Bemerkung, X_GK, Y_GK) %>% 
  st_drop_geometry()

result <- trees_2017_new_ID %>%
  left_join(trees_2013_new_ID %>% select(ID_Objekt_neu, Objekt), by = "ID_Objekt_neu", multiple = "all") %>%
  mutate(Objekt = coalesce(Objekt.y, Objekt.x)) %>%
  select(-Objekt.x, -Objekt.y) %>% 
  mutate(ID_Objekt_neu = ifelse(Objekt == "Buche" & ID_Objekt_neu == "59/L/1/20", "59/L/1/22", ID_Objekt_neu)) %>% 
  rename(Baumart = Objekt) %>% 
  select(ID_Objekt_neu, ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer, Baumart, Abgestorben, Kronenansatz, BHD, Höhe, Bemerkung, X_GK, Y_GK)
head(result)  

write.csv(result, "D:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_2017_aktuellster_Stand.csv")

sf_baumdaten_2017_aktuell <- st_as_sf(result, coords = c("X_GK", "Y_GK"), crs = 31464)

st_write(sf_baumdaten_2017_aktuell, "D:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Baumdaten_2017_aktuell.gpkg", driver = "GPKG", append = F)






## 3. Daten 2002 ----------------------------------------------------------------------------------------------------------

data_2002 <- st_read("E:/Alina/Stammfußkarte_ArcGIS_new/2002_BHD_Puffer.shp")

names(data_2002)


data_2002_transformed <- data_2002 %>% 
  #' select only necessary columns:
  select(UTM_rechts, UTM_hoch, stehend_o_, Transektnu, Transektab, Probequadr, Objektnumm, au.erhal, Objekt, Brusthoehe, Kronenansa, Gesamthoeh, lebend_o_t, Bemerkung, Hoehe_NN) %>% 
  rename(ID_Transekt = Transektnu,
         Transektabschnitt = Transektab, 
         Probequadrat = Probequadr,
         außerhalb_Transekt = au.erhal,
         Objektnummer = Objektnumm,
         Baumart = Objekt,
         BHD = Brusthoehe,
         Höhe = Gesamthoeh,
         Abgestorben = lebend_o_t,
         Liegend = stehend_o_,
         Kronenansatz = Kronenansa,
         UTM_X = UTM_rechts,
         UTM_Y = UTM_hoch, 
         Höhe_NN = Hoehe_NN) %>% 
  #' convert the contents of some attributes
  mutate(
    Liegend = case_when(
      Liegend == "liegend" ~ TRUE,
      is.na(Liegend) ~ FALSE),
    außerhalb_Transekt = case_when(
      außerhalb_Transekt == "x" ~ TRUE,
      außerhalb_Transekt == "+" ~ TRUE,
      außerhalb_Transekt == "a" ~ TRUE,
      is.na(außerhalb_Transekt) ~ FALSE),
    Abgestorben = case_when(
      Abgestorben == "tot" ~ TRUE,
      is.na(Abgestorben) ~ FALSE), 
    Baumart = case_when(
      Baumart == "Grauweide" ~ "Weide",
      Baumart == "Öhrchenweide" ~ "Weide",
      Baumart == "Salweide" ~ "Weide",
      TRUE ~ Baumart)
  )

#' Keeping only standing, existing trees in 2002 for areas not covered in 2017:

ID_Trans_List_2017 <- unique(trees_2017_standing_existing_BHD$ID_Transekt)

head(data_2002_transformed)

trees_2002_standing_existing_BHD <- data_2002_transformed %>% 
  filter(Baumart %in% c("Tanne", "Fichte", "Buche", "Nadelholz", "Spitzahorn", "Bergahorn", "Totholz", "Vogelbeere", "Aspe",
                        "Birke", "Sandbirke", "Weide", "Linde", "Moorbirke", "Laubholz"),
         Liegend != "TRUE",
         BHD >= 7,
         !ID_Transekt %in% ID_Trans_List_2017,
         !st_is_empty(geometry))


#' prepare the ID for each tree:
selected_cols <- c("Transektabschnitt", "Probequadrat")

trees_2002_new_ID <- trees_2002_standing_existing_BHD %>% 
  mutate(across(all_of(selected_cols), ~ ifelse(is.na(.), " ", .))) %>% 
  unite(ID, c(ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer), sep = "/", na.rm = T, remove = F) %>% 
  select(ID, ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer, Baumart, Abgestorben, Kronenansatz, BHD, Höhe, Bemerkung, UTM_X, UTM_Y) %>% 
  st_drop_geometry()

head(trees_2002_new_ID)

sf_baumdaten_2002 <- st_as_sf(trees_2002_new_ID, coords = c("UTM_X", "UTM_Y"), crs = 25832)
sf_baumdaten_2002_aktuell <- sf_baumdaten_2002 %>% 
  filter(ID_Transekt %in% c(60, 63, 21, 22, 56, 58))

mapview(sf_baumdaten_2002)
str(baumdaten_2002)

st_write(sf_baumdaten_2002, "E:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_2002.gpkg", driver = "GPKG")
st_write(sf_baumdaten_2002_aktuell, "E:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_2002_aktuell.gpkg", driver = "GPKG")



## 4. Daten Sallergäng und Scheuereck ----------------------------------------------------------------------------------------------------------

data_1999 <- st_read("E:/Waldstruktur Bodendaten/HTO Referenzflächen/bdrf/Sallerhaeng_Und_Scheuereck/bdrf_sallerhaeng_scheuereck.gpkg")
mapview(data_1999)

names(data_1999)


data_1999_transformed <- data_1999 %>% 
  #' select only necessary columns:
  select(-c(Jahr, BA_Kuerzel, layer, path)) %>% 
  rename(ID_Transekt = Transekt,
         Objektnummer = BNr,
         Baumart = BA,
         Höhe = Hoehe,
         Abgestorben = abgestorben,
         UTM_X = Rechtswert,
         UTM_Y = Hochwert) %>% 
  #' convert the contents of some attributes
  mutate(
    Abgestorben = case_when(
      Abgestorben == "true" ~ TRUE,
      Abgestorben == "false" ~ FALSE,
      is.na(Abgestorben) ~ FALSE)
  )

#' Keeping only standing, existing trees in 2002 for areas not covered in 2017:

ID_Trans_List_1999 <- c(96, 82, 83, 84, 85, 71, 72, 73, 75)

trees_1999_standing_existing_BHD <- data_1999_transformed %>% 
  filter(BHD >= 7, !st_is_empty(geom))

#' prepare the ID for each tree:
selected_cols <- c("Transektabschnitt", "Probequadrat")

trees_1999_new_ID <- trees_1999_standing_existing_BHD %>% 
  mutate(across(all_of(selected_cols), ~ ifelse(is.na(.), " ", .))) %>% 
  unite(ID, c(ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer), sep = "/", na.rm = T, remove = F) %>% 
  select(ID, ID_Transekt, Transektabschnitt, Probequadrat, Objektnummer, Baumart, Abgestorben, Kronenansatz, BHD, Höhe, Bemerkung, UTM_X, UTM_Y)

head(trees_1999_new_ID)

sf_baumdaten_1999 <- trees_1999_new_ID
sf_baumdaten_1999_aktuell <- sf_baumdaten_1999 %>% 
  filter(ID_Transekt %in% ID_Trans_List_1999)

mapview(sf_baumdaten_1999_aktuell)

st_write(sf_baumdaten_1999, "E:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_1999.gpkg", driver = "GPKG")
st_write(sf_baumdaten_1999_aktuell, "E:/Waldstruktur Bodendaten/HTO Referenzflächen/Geodaten/Baumdaten/Daten_stehende_Bäume_1999_aktuell.gpkg", driver = "GPKG")
