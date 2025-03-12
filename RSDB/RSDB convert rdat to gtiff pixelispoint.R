#' works only with Version 1.8-31 (development version of terra) You can install it with
install.packages('terra', repos='https://rspatial.r-universe.dev')

library(terra) 
library(RSDB)

#' function to convert rdat data and convert it to geotifff with pixelispoint
convert_rdat_to_gtiff <- function(input_file_path, output_file_path){
  r <- read.rdat(input_file_path)
  raster_data <- rast(r)
  metags(raster_data) <- cbind("AREA_OR_POINT", "Point", "")
  writeRaster(raster_data, filename = output_file_path, gdal = c("COMPRESS=LZW", "TILED=YES"), overwrite = T)
  
}

convert_rdat_to_gtiff(input_file_path = "C:/Users/Rieser/Desktop/ALS_2023_07_1m_DTM.rdat",  
                      output_file_path = "C:/Users/Rieser/Desktop/ALS_2023_DTM.tif")

convert_rdat_to_gtiff(input_file_path = "C:/Users/Rieser/Desktop/ALS_2023_07_1m_DSM.rdat",  
                      output_file_path = "C:/Users/Rieser/Desktop/ALS_2023_DSM.tif")

convert_rdat_to_gtiff(input_file_path = "C:/Users/Rieser/Desktop/ALS_2023_07_1m_CHM.rdat",  
                      output_file_path = "C:/Users/Rieser/Desktop/ALS_2023_CHM.tif")



