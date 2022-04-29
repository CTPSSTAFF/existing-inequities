# Raster processing
library(tidyverse)
library(sf)
library(stars)

# INPUTS ####
# read in census geog layers
mpo_census_tracts<- st_read("output/demographic_data.gpkg", layer= "tracts_acs_dec_2020") %>% 
  st_transform(3857)
boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(3857) # needs to be psudo-mercator for raster operations

# read in geotiff
access_layer<- read_stars("data/ConveyalRuns/HealthCareTests/HealthCare_NonEmergency_AMPeak_TransitBusRT_45min_50pct.geotiff") 
destination <- "Non-emergency Healthcare"
time_period <- "AM Peak"
modes <- "Transit (Bus and Rapid Transit only)"
travel_time <- "45 minutes"

access_layer_comp <- read_stars("data/ConveyalRuns/HealthCareTests/HealthCare_NonEmergency_AMPeak_Drive_P50_C45.geotiff")
modes_comp <- "Drive"

access_layer_comp2<- read_stars("data/ConveyalRuns/HealthCareTests/HealthCare_NonEmergency_AMPeak_TransitAll_P50_C45.geotiff")
modes_comp2 <- "TransitAll"

#plot(access_layer)

# CROP Access Opps to the MPO Boundary ####
# crop raster by mpo census geog
access_layer_crop <- st_crop(access_layer, boundary)
access_layer_comp_crop <- st_crop(access_layer_comp, boundary)
access_layer_comp2_crop <- st_crop(access_layer_comp2, boundary)
#plot(access_layer_crop)

# Aggregate by Population group ####
# http://132.72.155.230:3838/r/combining-rasters-and-vector-layers.html#extracting-to-polygons-single-band
test <- access_layer_crop %>% 
  aggregate(mpo_census_tracts, mean, na.rm=T) %>% 
  st_as_sf()
colnames(test)[1]<- "opps"

test_comps <- access_layer_comp %>% 
  aggregate(mpo_census_tracts, mean, na.rm=T) %>% 
  st_as_sf()
colnames(test_comps)[1]<- "opps_comp"

test_comps2 <- access_layer_comp2_crop %>% 
  aggregate(mpo_census_tracts, mean, na.rm=T) %>% 
  st_as_sf()
colnames(test_comps2)[1]<- "opps_comp2"

test2<- mpo_census_tracts %>% 
  st_join(test, join = st_equals) %>% 
  st_join(test_comps, join= st_equals) %>% 
  st_join(test_comps2, join= st_equals) %>% 
  mutate(
    people_opps = pop_dec*opps,
    opps_per_person = ifelse(pop_dec  ==0, 0, opps/pop_dec),
    # adult_min_opps = minority_adult*opps,
    # adult_nonmin_opps = nonminority_adult*opps,
    min_opps = minority*opps,
    min_opps_comp = minority*opps_comp,
    min_opps_comp2 = minority*opps_comp2,
    nonmin_opps = nonminority*opps,
    nonmin_opps_comp = nonminority*opps_comp,
    nonmin_opps_comp2 = nonminority*opps_comp2,
    lowinc_opps = lowinc*opps)%>%
  mutate(id= row_number())
  #select(GEOID, ends_with("_opps"))

ggplot(test2)+
  geom_point(aes(x=opps, y= pop_dec), alpha= .5)+
  geom_point(aes(x=opps, y= minority), color= "purple", alpha= .7)+
  geom_point(aes(x=opps, y= lowinc), color= "dark green", alpha=.7)+
  theme_minimal()
  
ggplot(test2)+
  # geom_point(aes(y= id, x= people_opps, alpha= .5))+
  geom_segment(aes(y=id, yend= id, x= min_opps, xend= nonmin_opps), alpha= .6)+
  geom_point(aes(y= id, x= min_opps), color= "purple", alpha= .7)+
  geom_point(aes(y= id, x= nonmin_opps),color= "orange", alpha= .7)+
  #geom_point(aes(y= id, x= lowinc_opps), color= "dark green", alpha= .7)+
  theme_minimal()

test3 <- test2 %>% 
  st_drop_geometry() %>% 
  summarize(#adult_min_opps= sum(adult_min_opps, na.rm=T),
            #adult_nonmin_opps = sum(adult_nonmin_opps, na.rm=T),
            min_opps= sum(min_opps, na.rm=T),
            nonmin_opps= sum(nonmin_opps, na.rm=T),
            min_opps_comp= sum(min_opps_comp, na.rm = T),
            nonmin_opps_comp = sum(nonmin_opps_comp, na.rm = T),
            min_opps_comp2 = sum(min_opps_comp2, na.rm = T),
            nonmin_opps_comp2 = sum(nonmin_opps_comp2, na.rm=T))
ggplot(test3)+
  geom_segment(aes(y=1, yend= 1, x= min_opps, xend= nonmin_opps), alpha= .6)+
  geom_point(aes(y= 1, x= min_opps), color= "purple", alpha= .7)+
  geom_point(aes(y= 1, x= nonmin_opps),color= "orange", alpha= .7)+
  geom_segment(aes(y=2, yend= 2, x= min_opps_comp, xend= nonmin_opps_comp), alpha= .6)+
  geom_point(aes(y= 2, x= min_opps_comp), color= "purple", alpha= .7)+
  geom_point(aes(y= 2, x= nonmin_opps_comp),color= "orange", alpha= .7)+
  geom_segment(aes(y=3, yend= 3, x= min_opps_comp2, xend= nonmin_opps_comp2), alpha= .6)+
  geom_point(aes(y= 3, x= min_opps_comp2), color= "purple", alpha= .7)+
  geom_point(aes(y= 3, x= nonmin_opps_comp2),color= "orange", alpha= .7)+
  theme_minimal()
# mapview(test)



# Contours ####
# TODO: Figure out how to do filled contours and plot in ggplot
# TODO: set up breaks to be within min and max of the destination opps
# maybe see http://132.72.155.230:3838/r/combining-rasters-and-vector-layers.html#raster-to-contour
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



