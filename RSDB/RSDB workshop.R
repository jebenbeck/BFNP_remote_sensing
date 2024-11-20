# Script for working with the RSDB instance of the NPBW to process LiDAR data

# Author: Paul Magdon
# Date: 2022-05-03


# Prepare your environment

## Install required packages at the first run only

if(!require("remotes")) install.packages("remotes")
remotes::install_github("environmentalinformatics-marburg/rsdb/r-package")
install.packages('lidR')

# Load the required packages

library(RSDB)
library(lidR)
library(ggplot2)
library(gridExtra)


# Connect to the RSDB Server

## Provide the credentials in an local text file on your computer
secret<-"jakob.rieser:password"
#source('C:/Users/pmagdon/Documents/rsdb_credentials.R')

## Connect to the server
db<- RemoteSensing$new("https://foresteye-server.de:8082",secret) # remote server

## Get the Points Of Interesst (POI) which are the centers of the transect plots
poi <- db$poi_group(group_name='poi_npbw')
head(poi)

center<-poi[which(poi$name=="T3_2"),]

ext <- RSDB::extent_radius(center$x,center$y,r=100)
ext

# Select a ALS acquisition

als_2007 <- db$pointcloud('ALS2007_05')
als_2017 <- db$pointcloud('ALS2017_00')


# Querry the points

pts_2007 <- als_2007$points(ext=ext,normalise = "ground")
pts_2017 <- als_2017$points(ext=ext,normalise = "ground")
head(pts_2017)

pts_2017$scanAngleRank<-0

lidar_2007 <- RSDB::as.LAS(pts_2007)
lidar_2017 <- RSDB::as.LAS(pts_2017)

# Set Coordinate reference system 

projection(lidar_2007)<- "EPSG:32632"
projection(lidar_2017)<- "EPSG:32632"

lidar_2017
lidar_2007

# plot in 3D 
# For R 4.2 on Win10 there is a bug in the rgl package which can be fixed by
options(rgl.debug = TRUE)
plot(lidar_2007)
plot(lidar_2017)


# Compare the two scans using a cross-section plot

las_tr_2017 <- clip_transect(lidar_2017,p1=c(center$x-50, center$y),p2=c(center$x+50,center$y), width = 5, xz = TRUE)

p1<-ggplot(las_tr_2017@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))+
  ggtitle('ALS 2017')

las_tr_2007<- clip_transect(lidar_2007,p1=c(center$x-50, center$y),p2=c(center$x+50,center$y), width = 5, xz = TRUE)

p2<-ggplot(las_tr_2007@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))+
  ggtitle('ALS 2007')

grid.arrange(p1, p2, ncol=2)


## Calculate LiDAR MEtrics for the extent

fhd_2007<- als_2007$indices(ext,functions = 'BE_FHD')
fhd_2016<- als_2017$indices(ext,functions = 'BE_FHD')
fhd_2007
fhd_2016

df <- pointcloud$indices(list(p1=p), pointcloud$index_list$name)
