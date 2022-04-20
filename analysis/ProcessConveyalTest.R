# Raster processing
library(raster)
library(rgdal)


# read in census geog layer
mpo_census_tracts
mpo_census_bgr

bounary

# read in geotiff
access_layer<- raster(".geotiff")

# crop raster by mpo census geog
access_layer <- crop(access_layer, boundary)

plot(access_layer, main= "Access Layer Cropped by MPO boundary",
     col = rev())
plot(bounary, add= TRUE)
  


