library(sgsR)
library(terra)
library(mapview)
library(sf)
library(dplyr)
library(tidyr)
library(pbapply)
library(ggplot2)


results_path <- "C:/Users/jakob/OneDrive/BFNP/Projects/Forest Ecosystem Monitoring/R Scripts/Reference area representativeness/Results/"

## 1. Import and preprocess all datasets -------------------------------------------------------------------------------



### The area of interest ----


AOI_BFNP <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/BFNP divisions.gpkg", 
                    layer = "Expansion areas", quiet = F) %>% 
  filter(grepl('1997|1970', Name)) 


### The habitat types polygons ----


Forest_areas <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Habitats LULC.gpkg",
                        quiet = T) %>% 
  filter(grepl('Nadel|Totholz|Laub|Misch|Latsche', Class.German)) %>% 
  st_intersection(AOI_BFNP)


### The ALS metrics ----


ALS_metrics <- rast("E:/ALS_metrics_2017_prj.tif")

ALS_metrics_subset <- ALS_metrics %>% 
  subset(subset = grep('BE_H_|BE_FHD|BE_PR|vegetation_coverage_05', 
                       x = names(ALS_metrics), ignore.case = T, value = T)) %>% 
  mask(Forest_areas)


### The tree type coverage raster ----


Tree_type_coverage <- rast("E:/Single tree polygons 2017/Cover Rasters 10m/Mosaic/Coverage_tree_types.tif") %>% 
  resample (ALS_metrics_subset) %>% 
  subset(subset = "snag", negate = T) %>% 
  mask (Forest_areas)

#' combine the TTC and ALS rasters:
ALS_TTC_metrics <- c(ALS_metrics_subset, Tree_type_coverage)


### The ground reference points and polygons ----


# ---- Different Bioklim datasets ---- #

#' Bioklim 2016 points:
Points_bioklim_2016 <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Transects Bioclim/Bioklim_points_2016_BFNP.gpkg", 
                                    quiet = T) %>% 
  st_zm(drop = TRUE, what = "ZM")

#' Bioklim 2006 points:
Points_bioklim_2006 <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Transects Bioclim/Bioklim_points_2006.gpkg", 
                              quiet = T) %>% 
  st_intersection(AOI_BFNP)

#' Biodiv points:
Points_biodiv <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Transects Bioclim/Biodiv_points.gpkg", 
                              quiet = T) %>% 
  st_intersection(AOI_BFNP)

#' Bioklim transect polygons:
Points_transect <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Transects Bioclim/Bioklim_transects_plots.gpkg",
                           quiet = T) %>% 
  st_transform(crs = crs(ALS_TTC_metrics)) %>% 
  terra::rasterize(y = ALS_TTC_metrics, field = "Square_ID", background = NA) %>% 
  as.points(values=F, na.rm=TRUE, na.all=FALSE) %>% 
  st_as_sf()
plot(Points_transect)


# ---- Inventory points ---- #

#' load the 800-m inventory points:
Points_inventory_800 <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Base data/Bavarian Forest National Park/Inventory/Inventory_points_800x800.gpkg",
                                quiet = T) %>% 
  select("geom") %>% 
  st_transform(crs = crs(ALS_TTC_metrics))


# ---- Different reference and monitoring areas ---- #

#' HTO reference polygons:
Points_HTO <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/HTO_reference_areas_combined.gpkg", quiet = T) %>% 
  filter(Type_plot == "Plot") %>% 
  terra::rasterize(y = ALS_TTC_metrics, field = "Type_plot", background = NA) %>% 
  as.points(values=F, na.rm=TRUE, na.all=FALSE) %>% 
  st_as_sf()

#' Large Scale Plot monitoring polygon:
Points_LSP <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Other data/Messflugwoche 2022/Large Scale Plot/Plot 20ha/Plot_TB_20ha_ETRS_final_JTSK.shp", 
                                   quiet = T) %>%
  st_transform(crs = crs(ALS_TTC_metrics)) %>% 
  terra::rasterize(y = ALS_TTC_metrics, field = "Id", background = NA) %>% 
  as.points(values=F, na.rm=TRUE, na.all=FALSE) %>% 
  st_as_sf()

#' Mittelsteighütte monitoring polygon:
Points_MSH <- st_read("C:/Users/jakob/OneDrive/BFNP/Data/Other data/Messflugwoche 2022/Mittelsteighütte/Dauerbeobachtungsflächen.gpkg", 
                      quiet = T) %>%
  st_transform(crs = crs(ALS_TTC_metrics)) %>% 
  terra::rasterize(y = ALS_TTC_metrics, background = NA) %>% 
  as.points(values=F, na.rm=TRUE, na.all=FALSE) %>% 
  st_as_sf()


# ---- Combine all reference datasets in a list ---- #
list_sample_pts <- list("Bioklim 2016" = Points_bioklim_2016, 
                        "Bioklim 2006" = Points_bioklim_2006, 
                        "Biodiv" = Points_biodiv, 
                        "Inventory 800m" = Points_inventory_800, 
                        "Reference areas" = rbind(Points_HTO, Points_LSP, Points_MSH), 
                        "Bioklim transects" = Points_transect)



## 2. K-means clustering -----------------------------------------------------------------------------------------------





### K-means cluster ----

#' only calculate when file does not already exist:
if (file.exists(paste0(results_path,"ALS_TTC_strata.tif"))) {
  
  #' load stratified raster:
  ALS_TTC_strata <- rast(paste0(results_path,"ALS_TTC_strata.tif"))
  
} else {
  
  #' Apply k-means clustering to ALS metrics:
  ALS_TTC_strata <- strat_kmeans(mraster = ALS_TTC_metrics, 
                                 nStrata = 10, 
                                 iter = 1000, 
                                 algorithm = "MacQueen",
                                 plot = T) 
  
  #' write strat raster to disk:
  writeRaster(ALS_TTC_strata, paste0(results_path,"ALS_TTC_strata.tif"))

}


### Reclassify the raster values ----

#' reclassification is performed for better visualization
#' make matrix to use for the reclassification

m <- c(0.5, 1.5, 9,
       1.5, 2.5, 2,
       2.5, 3.5, 3,
       3.5, 4.5, 4, 
       4.5, 5.5, 10, 
       5.5, 6.5, 6, 
       6.5, 7.5, 8, 
       7.5, 8.5, 1, 
       8.5, 9.5, 5, 
       9.5, 10.5, 7)

rclmat <- matrix(m, ncol = 3, byrow=TRUE)

ALS_TTC_strata_sorted <- classify(ALS_TTC_strata, rclmat)


#' write strat raster to disk:
writeRaster(ALS_TTC_strata_sorted, 
            paste0(results_path, "ALS_TTC_strata_edit.tif"), 
            overwrite = T)



## 3. Calculate Representation -----------------------------------------------------------------------------------------



#' calculate the representation of the existing reference points based on the k-means classes of the ALS and TTS data

#' make function iterating through all sets of points, that should be tested
calc_rep <- function(i){
  return(calculate_representation(sraster = ALS_TTC_strata_sorted, 
                                  existing = list_sample_pts[[i]]) %>%
           mutate(sample = names(list_sample_pts)[[i]])) 
}

#' apply function:
rep_total <- pblapply(seq_along(list_sample_pts), calc_rep) %>% 
  #' combine all tables into 1:
  bind_rows() %>% 
  pivot_longer(cols = c("srasterFreq", "sampleFreq"), names_to = "Freq_type", values_to = "Freq") %>% 
  mutate(strata = as.factor(strata))

rep_total



## 4. Visualisation ----------------------------------------------------------------------------------------------------------



plot_fun <- function(table, title) {
  plot_freq <- ggplot(table, aes(x = strata, y = Freq, fill = Freq_type)) + 
    geom_bar(stat = "identity", width = 0.9, position = "dodge") + 
    facet_wrap(facets = "sample") +
    scale_x_discrete(expand = c(0, 0), name = "Strata") + 
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,0.6), 
                       name = "Frequency") +
    scale_fill_discrete(labels = c("Sample", "Population")) +
    ggtitle(title) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "right")
  
  ggsave(filename = paste0(title, ".png"),
         plot = plot_freq, device = "png",
         path = results_path,
         width = 200, height = 100, units = "mm", dpi = 300)
  
  return(plot_freq)
}

plot_fun(rep_total, "Representation based on ALS and TTC metrics data")
