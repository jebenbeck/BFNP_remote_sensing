
## 1. Reproject LasCatalog ------------------------------------------------------------------------------------------------


#' This is a function to transform the coordinate system of a lascatalog. It is specialized for large-scale ALS data (many flight lines / tiles).
#' While it can transform from/to any system implemented in st_transform, it is specialized for converting GK4 to UTM32
#' by using transformation grid from the German surveying authorities. Input shall be a las catalog (folder with many LAZ/LAS files), 
#' output will be LAZ files. The results will be the exact same files but reprojected to the correct epsg code. 
#' There will be no retiling or alterations of chunks.


reproject_lascatalog <- function(lascatalog, input_epsg, output_epsg, output_path, parallel = FALSE, n_cores = 2)  {
  
  #' check projection if needed:
  if (is.na(st_crs(lascatalog))) {
    warning(paste("LasCatalog does not have an assigned projection, input_epsg", input_epsg, "will be used"), call. = F, immediate. = T)
    st_crs(lascatalog) <- input_epsg
  }
  
  #' apply options to lascatalog
  opt_output_files(lascatalog) <- paste0(output_path, "/{ORIGINALFILENAME}")
  opt_laz_compression(lascatalog) <- TRUE
  opt_chunk_buffer(lascatalog) <- 0
  opt_chunk_size(lascatalog) <- 0
  opt_independent_files(lascatalog) <- TRUE
  
  #' special check if transformation is from GK4 to UTM32
  if (input_epsg == "EPSG:31468" & output_epsg == "EPSG:25832") {
    #' activate network connection
    sf_proj_network(enable = T)
    message("Proj connection to network enabled:", sf_proj_network())
    #' get transformation options available with grid:
    options <- sf_proj_pipelines(source_crs = "EPSG:31468", target_crs = "EPSG:25832")
    message ("Transformation option in use:", options[1,]$definition)
    #' ckech if transformation grid exists:
    if (file.exists(paste0(sf_proj_search_paths(), "/de_adv_BETA2007.tif")) == T) {
      message ("BETA 2007 transformation grid found and will be used")
    } else {
      warning("BETA 2007 transformation grid not found, accurracy of transformation will be compromized", call. = F, immediate. = T)
    }
  }
  
  #' function to reproject las data:
  reproject = function(las){
    las_trans = sf::st_transform(las, crs = output_epsg)
    return(las_trans)
  }
  
  #' plan parallel processing
  if (parallel == TRUE) {
    plan(multisession, workers = n_cores)
    message("Parallel processing will be used with", n_cores, "cores")
  } else {
    warning("No parallel processing in use", call. = F, immediate. = T)
  }
  
  #' apply function to lascatalog:
  reprojected_catalog = catalog_map(lascatalog, reproject)
  return(reprojected_catalog)
}



## 2. Generate footprint polygons of lascatalog ------------------------------------------------------------------------


catalog_to_polygons <- function(clg) {
  ctg_polygons <- st_as_sf(clg) %>% 
    #' convert filename to tile name:
    mutate(Tile.name = tools::file_path_sans_ext(basename(filename)), .before = everything()) %>% 
    #' remove unecessary attributes:
    select(-c(File.Signature, File.Source.ID, File.Creation.Year, File.Creation.Day.of.Year, GUID, Version.Major, Version.Minor, System.Identifier,
              Generating.Software, Header.Size, Offset.to.point.data, Number.of.variable.length.records, Point.Data.Format.ID, Point.Data.Record.Length,
              filename)) %>% 
    #' rename number of points:
    rename("Number.of.points" = "Number.of.point.records") %>% 
    #' restructure:
    relocate(CRS, .after = Tile.name) %>% 
    relocate(Number.of.points, .before = Number.of.1st.return) %>% 
    relocate(starts_with("Number.of"), .before = CRS)
  
  return(ctg_polygons)
}



## 3. Calculate point density of lascatalog files ----------------------------------------------------------------------

catalog_statistics <- function(lascatalog, parallel = F, n_cores = 2){
  
  #' set catalog options:
  opt_select(lascatalog) <- "xy" #' read only xy because they are the only attributes of interest
  opt_output_files(lascatalog) <- "" #' receive results in R environment 
  opt_independent_files(lascatalog) <- TRUE
  
  #' function to get point density of las data:
  calc_statistics = function(chunk) {
    #return(chunk)
    las <- readLAS(chunk)
    if (is.empty(las)) return(NULL)
    #' calculate point density:
    density = density(las) 
    #' calculate area covered by convex hull (not bbox):
    area = area(las)
    #' extract tile name from file (for merging information to other data later):
    tilename = tools::file_path_sans_ext(basename(chunk@files))
    #' extract extent of full tile:
    extent_tile <- chunk@bbox
    #' make data frame with necessary information:
    df <- tibble(Tile.name = tilename, 
                 Point.density = density, 
                 Area.covered = area,
                 Tile.max.X = extent_tile[1, 2],
                 Tile.min.X = extent_tile[1, 1],
                 Tile.max.Y = extent_tile[2, 2],
                 Tile.min.Y = extent_tile[2, 1])
    return(df)
  }
  
  #' plan parallel processing
  if (parallel == TRUE) {
    plan(multisession, workers = n_cores)
    message("Parallel processing will be used with", n_cores, "cores")
  } else {
    warning("No parallel processing in use", call. = F, immediate. = T)
  }
  
  #' apply function to catalog:
  statistics <- catalog_apply(lascatalog, calc_statistics)
  #' merge the results to a single data frame:
  statistics_merged_df <- bind_rows(statistics)
}



## 4. Retiling ---------------------------------------------------------------------------------------------------------


#' This function retiles lascatalogs, so many las files corresponding to a single acquisition. Sometimes, ALS las data is 
#' not stored in rigid tiles but in parts of flight lines which can overlap. This is not good, because if you want to 
#' know all points over a certain location within your AOI, you need to consider multiple files to get all of them.
#' 
#' This script takes any lascatalog (directory with many las/laz files) and restructures it to match a common grid of 
#' 1x1 km tiles in the coordinate system.


catalog_retiling <- function(lascatalog, tile_alignment = c(0,0), tile_size = 1000, buffer_size = 0, output_path, 
                             filename_convention = "{XLEFT}_{YBOTTOM}", laz_compression = T){
  #' set catalog options:
  opt_chunk_alignment(lascatalog) <- tile_alignment
  opt_chunk_size(lascatalog) <- tile_size 
  opt_chunk_buffer(lascatalog) <- buffer_size
  opt_laz_compression(lascatalog) <- laz_compression
  opt_output_files(lascatalog) <- paste0(output_path, "/", filename_convention) 
  
  #' perform the retiling:
  ctg_retiled <- catalog_retile(lascatalog) 
}



## 5. Clip AOIs --------------------------------------------------------------------------------------------------------


#' to check the spatial accurracy between the different datasets, AOIs are used. This function generates multiple output
#' LAZ files based on input polygons (one file per polygon). 

catalog_clip_polygons <- function(lascatalog, input_epsg, output_path, filename_convention = "{ID}", polygons) {
  
  #' check projection if needed:
  if (is.na(st_crs(lascatalog))) {
    warning(paste("LasCatalog does not have an assigned projection, input_epsg", input_epsg, "will be used"), call. = F, immediate. = T)
    st_crs(lascatalog) <- input_epsg
  }
  
  if (st_crs(polygons) != st_crs(lascatalog)){
    warning("CRS of input polygons and lascatalog differ. Polygons are reprojected to match input CRS")
    polygons <- sf::st_transform(polygons,  crs = input_epsg)
  } 
  
  #' apply options to lascatalog
  opt_output_files(lascatalog) <- paste0(output_path, "/", filename_convention)
  opt_laz_compression(lascatalog) <- TRUE
  opt_independent_files(lascatalog) <- TRUE
  
  #' run clipping:
  clipped_catalog <- clip_roi(lascatalog, polygons)
  return(clipped_catalog)
}


## 6. Accurracy assessment ---------------------------------------------------------------------------------------------



### 6.1. Prepare GCPs ----


#' This function takes a list of txt-files generated by the CloudCompare point picking tool and converts them to a single 
#' geopackage. The points represent GCPs sampled in the point clouds within the pre-defined AOIs.

prepare_GCPs <- function(input_path, output_path, filename, polygons){
  
  #' list all txt files generated by CloudCompare point picking tool:
  file_list <- list.files(input_path, pattern = "\\.txt$", full.names = T)
  
  #' process the data:
  
  data <- lapply(file_list, read.table, header = F, sep = ",") %>%  # Read all files into a list 
    bind_rows() %>%     #' combine tha datasets
    rename_with(~ c("Description", "X", "Y", "Z")) %>%      #' rename columns 
    st_as_sf(coords = c("X", "Y"), crs = "EPSG:25832", remove = F) %>%      #' convert to spatial data
    st_join(AOIs["name"]) %>%       #' overlay GCPs with polygons to get info on AOI
    rename(AOI = name) %>% 
    group_by(AOI) %>% 
    mutate(ID = paste0(substr(AOI, 1, 3), "_", row_number()),  .before = everything()) %>%  #' generate an unique ID for all GCPs
    ungroup()
  
  #' export to disk:
  st_write(data, paste0(output_path, "/", filename, ".gpkg"), append = F) 
  
  return(data)
  
}


### 6.2. Merge GCPs with reference data ----


#' Merges the GCPs that should be tested (of one year) with their corresponding GCPs of the LDBV ALS data

merge_GCPs <- function(gcp_data_test, gcp_data_ref, export = F, filename = F, output_path = F) {
  
  #' calculate indices describing the nearest feature:
  nearest_indices <- st_nearest_feature(gcp_data_ref, gcp_data_test)
  
  # Add corresponding matches from the test data to the reference data
  gcp_data_ref$matchedID <- gcp_data_test$ID[nearest_indices]
  
  # Join attributes
  gcp_data_test_ref <- left_join(as.data.frame(gcp_data_test), as.data.frame(gcp_data_ref), 
                                 by = c("ID" = "matchedID"), suffix = c("", "_ref"), keep = F) %>%
    rename("X_obs" = "X", "Y_obs" = "Y", "Z_obs" = "Z") %>% 
    mutate(deltaX = X_obs - X_ref, 
           deltaY = Y_obs - Y_ref,
           deltaH = sqrt(deltaX^2 + deltaY^2),  # Horizontal Error
           deltaZ = Z_obs - Z_ref) %>% 
    filter(abs(deltaX) < 5,
           abs(deltaY) < 5,
           abs(deltaZ) < 5) %>%        #' filter points that are less the 5 m apart to their match
    select(ID, Description, AOI, X_obs, Y_obs, Z_obs, X_ref, Y_ref, Z_ref, deltaX, deltaY, deltaH, deltaZ, geom) %>% 
    st_as_sf() 
  
  if (export == T) {
    #' export to disk:
    st_write(gcp_data_test_ref, paste0(output_path, "/", filename, ".gpkg"), append = F) 
  }
    
  return(gcp_data_test_ref)

}


### 6.3. Generate boxplots ----


#' generates boxplots showing the difference between the GCPs from the test data to the reference data in X, Y and Z 
#' dimension per AOI.

aa_create_boxplots <- function(gcp_data, export = F, filename, output_path){
  
  #' re-format data for better visualization:
  data <- gcp_data %>% pivot_longer(cols = c(deltaX, deltaY, deltaZ), names_to = "Direction", values_to = "Difference") %>% 
    mutate(Direction = recode(Direction, "deltaX" = "X", "deltaY" = "Y", "deltaZ" = "Z"))
  
  #' generate the plot:
  bplot <- ggplot(data = data, aes(y = Difference, x = AOI)) +
    geom_hline(yintercept = 0, color = "darkgrey", linewidth = 0.5) +
    geom_point(position = position_jitter(width = 0.1), shape = 1, size = 0.1, color = "blue") +
    geom_boxplot(outliers = F, color = "black", fill = NA, width = 0.2) +
    facet_wrap(vars(Direction), nrow = 3, ncol = 1, shrink = F) + 
    labs (y = "Difference (meter)") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  plot(bplot)
  
  if (export == T) {
    #' export the plot to disk:
    ggsave(filename = paste0(filename, ".png"),
           plot = bplot, device = "png",
           path = output_path,
           width = 200, height = 200, units = "mm", dpi = 300)
  }
  
} 


### 6.4. Calculate accurracy metrics ----


aa_metrics <- function(gcp_data, export = F, filename, output_path){
  
  #' Convert data to long format for easier processing
  df_long <- gcp_data %>%
    st_drop_geometry() %>% 
    select(deltaX, deltaY, deltaZ, deltaH) %>%
    pivot_longer(cols = everything(), names_to = "direction", values_to = "error")
  
  #' Compute accuracy metrics using yardstick
  summary_table <- df_long %>%
    group_by(direction) %>%
    summarise(
      RMSE = round(rmse_vec(rep(0, n()), error), 3),
      MAE = round(mae_vec(rep(0, n()), error), 3),
      Bias = round(mean(error), 3),
      SD = round(sd(error), 3)
    ) %>%
    mutate(direction = recode(direction, "deltaX" = "X", "deltaY" = "Y", "deltaZ" = "Z", "deltaH" = "H"))

  #' Export table to csv
  if (export == T) {
    write.csv2(summary_table, file = paste0(output_path, "/", filename, ".csv"), row.names = F, dec = ".")
  }
  
  #' Print the final table
  return(summary_table)
  
}
