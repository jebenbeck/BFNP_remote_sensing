## Info ----------------------------------------------------------------------------------------------------------------

#' Author: Jakob Ebenbeck
#' Last updated: 07.11.2024
#' Status: Work in progress 

### Purpose of script ----

#' in the forest inventory campaign, photos of the plot were created using the OpenForis collect app. These are named 
#' with a random alphanumeric code. To be able to use them better, they should be renamed with the inventory plot ID
#' which can be done using this script  

#' the script takes the files, renames them and copies them into a new directory

### Required datasets ----

#' - folder with photos
#' - csv-table with photo names and new names that should be applied 

### Required packages ----

require(tidyverse)


## Code ----------------------------------------------------------------------------------------------------------------

# Read the table containing filenames and numbers (adjust the file path as needed)
# Table is in a CSV format with 2 columns 'filename' and 'new name'
input_file_names <- "N:/SG5/Waldstruktur/Waldinventur/Waldinventur2024/Daten/2024_finaler_Datensatz/plot_final.csv"
filename_column <- "plot_foto_file_name"
new_name_column <- "plot_id"

#' Define the directory where the files are located:
input_files_directory <- "C:/Users/Ebenbeck_J/Desktop/collect_upload_neu/Plot Fotos original"

#' Define the directory where the new files should be located:
output_files_directory <- "C:/Users/Ebenbeck_J/Desktop/collect_upload_neu/Plot Fotos renamed"

#' check if output directory exists. If not, create it:
if (!dir.exists(output_files_directory)) {
  dir.create(output_files_directory)
  cat("output directory created")
}

#' import csv file and preprocess:
file_mapping <- read_csv(input_file_names) %>% 
  select({{filename_column}}, {{new_name_column}}) %>% 
  rename("filename" = {{filename_column}}) 

#' List files in the input directory and create a tibble of file paths and filenames
files_in_directory <- tibble(
  file_path = list.files(input_files_directory, full.names = TRUE),
  filename = list.files(input_files_directory)
)

#' Join the pictures with the new names by filename:
files_to_rename <- files_in_directory %>%
  full_join(file_mapping, by = "filename") %>% 
  #' generate new file name for each file:
  mutate(new_file_path = file.path(output_files_directory, paste0(plot_id, ".jpg"))) 

View(files_to_rename)

#' copy and rename files to output directory 
walk2(files_to_rename$file_path, files_to_rename$new_file_path, ~ {
  if (file.exists(.x)) {  #' Check if the old file exists
    file.copy(.x, .y)     #' Rename the file
  } else {
    warning(paste("File does not exist:", .x))
  }
})
