devtools::install_github("citoverse/cito")  
devtools::install_github(repo = "TheoreticalEcology/EcoData", dependencies = F, build_vignettes = F)  

library(EcoData)
library(cito)
library(torch)
library(ggplot2)

#' BEWARE: the learning rate of a dnn highly influences the loss!
#' The traing liss shoulld always fall below the baseline loss

#' it's possible to directly split the data in the function into training and validation, to monitor the training and accuracy
#' early stopping can be used to manuaelly tell the model when to stop training (based on the validation loss) to avoid overfitting

#' with hyperparameter tuning it's possible to do k-fold cv directly in cito -> the model automatically uses the "best" trained 
#' version based on loss

#' it's possible to tunr zhr architecture of the models but it's not recommended
#' important to tune is the learning rate and the regionalization

#' it's possible to parallelize the tuning across several GPUS
#' you always should use vaidation split and early stopping because then the # of epochs do not matter (can be set very high!) 

#' with ALE() you get the local effects for all input parameters

#' with bootstrapping it's easy to implement e.g. 20 models
#' can also be parallelized


#' when working with rasters, cito expects 4 parameters: number of observations, number of bands, number of pixels in x and y direction (seperately)
#' when building the architecture: the convolutional layer alway learn the same, so we are using (it's state-of-the-art) 
#' pre-trained models for the convolutional layers which is why the conv. layers are replaced by pre trained models
#' it's possible to include different pretrained models
#' 
#' whe using pre-trained models it's recommendet to swith to use a GPU because they are computationally intensive

#' instead of loading all images into memory, we can just define paths to image data (e.g. rgb, lidar, etc)
#' data is then loaded in batches which can be defined by batchsize

#' custom loss functions have to be built when there are different types of datasets theat should be predicted (e.g. bernulli vs normal distribution)
#' thie is done by applying 3 different loss functions if we have 3 different prediction datasets
#' this is done using torch syntax