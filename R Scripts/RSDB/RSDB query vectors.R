library(RSDB)


### Connect to the RSDB Server ----

#' Provide the login-credentials in an local R file on your computer or via an object:
#' format: "username:password"
source("C:/Users/jakob/OneDrive/BFNP/Projects/Forest Ecosystem Monitoring/R Scripts/RSDB/RSDB credentials.R")

#' Get access to the database
db <- RemoteSensing$new("https://foresteye-server.de:8082", credentials) 


### Get the Raster Data ----

#' Select raster database:
Areas_NPBW.db <- db$vectordb("Areas")

#' Get the polygons:
Areas_NPBW.poly <- Areas_NPBW.db$getVectors()

#' look at data:
Areas_NPBW.poly
plot(Areas_NPBW.poly)
