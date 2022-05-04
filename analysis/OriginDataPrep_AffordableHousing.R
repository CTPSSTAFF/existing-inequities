library(tidyverse)
library(sf)

boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(26986)

#FUNCTIONS ####
source("functions/points_cleaning.R")



#https://www.massbuilds.com/map?panel=signup
# Read in shapefiles
if( dir.exists("data/temp")){sapply(paste0("data/temp/", list.files("data/temp")), unlink)}
unzip(paste0("data/massbuilds-shp-20220503-e2740c.zip" ), exdir= "data/temp")

mass_builds <- read_sf("data/temp/massbuilds-shp-20220503-e2740c.shp")

affordable_housing <- mass_builds %>% filter(STATUS == "completed") %>% 
  st_filter(boundary, .predicate = st_within) %>% 
  mutate(id = row_number()) %>% 
  select(id, weight = AFFRD_UNIT) %>% 
  filter(weight >0)

mapview::mapview(affordable_housing, cex= 'weight')


# SAVE DATA FOR CONVEYAL ####
# note: Conveyal can't have any other numeric fields beside a weight. 
# if there is an id or an FID (default saving with shp driver) then that id becomes the weight and throws off access opportunity counts
ah_conveyal <- affordable_housing %>% 
  # rename(geometry= geom) %>%
  select(geometry, id, weight) %>% 
  st_transform(4269)

ah_csv<- ah_conveyal %>%   
  pt_to_csv("output/afforablehousing.csv")
