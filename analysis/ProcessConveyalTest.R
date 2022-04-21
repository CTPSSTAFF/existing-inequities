# Raster processing
library(tidyverse)
library(sf)
library(stars)

# INPUTS ####
# read in census geog layers
mpo_census_tracts
mpo_census_bgr

boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(3857) # needs to be psudo mercator for raster opperation

# read in geotiff
access_layer<- read_stars("data/ConveyalRuns/HealthCareTests/HealthCare_NonEmergency_AMPeak_TransitBusRT_45min_50pct.geotiff") 
destination <- "Non-emergency Healthcare"
time_period <- "AM Peak"
modes <- "Transit (Bus and Rapid Transit only)"
travel_time <- "45 minutes"
#plot(access_layer)

# CROP Access Opps to the MPO Boundary ####
# crop raster by mpo census geog
access_layer_crop <- st_crop(access_layer, boundary)
#plot(access_layer_crop)

# Aggregate by Population group ####

# Contours ####
# TODO: Figure out how to do filled contours and plot in ggplot
# TODO: set up breaks to be within min and max of the destination opps
# st_contour creates lines at break values
access_contours <- st_contour(access_layer,
                              na.rm=T,
                              contour_lines = T,
                              breaks = seq(10,500, by= 50) )
colnames(access_contours)[1]<- "value"
# plot(access_contours)
# plot(access_layer_crop)
# filled.contour(access_layer_crop, add=T)


# Visualize ####

ggplot()+
  geom_stars(data=access_layer_crop, alpha = .7)+
  geom_sf(data=access_contours, aes(color = value),size=.8, show.legend = F)+
  # geom_contour(data=access_layer_crop)+
  geom_sf(data=boundary,size=1,color="light gray", fill= NA)+
  coord_sf()+
  scale_fill_viridis_c(option = "D", na.value = "transparent",
                       name = paste0(destination, "\n Opportunities Accessible"))+
  scale_color_viridis_c(option = "D", na.value = "transparent",
                       name = paste0(destination, "\n Opportunities Accessible"))+
  ggtitle(paste0("Access to ", destination, "\nwith ",
                                    modes, "\nin ", travel_time))+
  labs(caption= paste0("Time period: ", time_period))+
  theme_void()



