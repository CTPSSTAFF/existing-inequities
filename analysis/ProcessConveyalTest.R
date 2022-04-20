# Raster processing
library(tidyverse)
library(sf)
library(stars)

# read in census geog layer
mpo_census_tracts
mpo_census_bgr

boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(26986)

# read in geotiff
access_layer<- read_stars("data/ConveyalRuns/HealthCareTests/HealthCare_NonEmergency_AMPeak_TransitBusRT_45min_50pct.geotiff") %>% 
  st_transform(26986)
plot(access_layer)
# crop raster by mpo census geog
access_layer_crop <- st_crop(access_layer, boundary)

plot(access_layer_crop, main= "Access Layer Cropped by MPO boundary")
plot(boundary,col="blue", add= TRUE)
  


