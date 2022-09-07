# Engagment material map data prep
library(sf)
library(tigris)
library(mapview)
library(tidyverse)
# BASE LAYERS ####
mpo_boundary <- st_read("data/boundary.shp")
community_types
# county_list <- c(009, 017, 027, 021, 023)

# roads <- roads("MA", county =county_list[1],  year = 2020)
# list_counties("MA")
# counties <- counties("MA")

# mapview(roads)

# ACCESS DATA ####
library(stars)
# healthcare_nonemg_access <- read_rds("app/data/healthcareNonEmg_access.rds")
# healthcare_emg_access <- read_rds("app/data/healthcareEmg_access.rds")
# jobs_access <- read_rds("app/data/jobs_access.rds")
essentialplaces_access <- read_rds("app/data/essentialplaces_access.rds")
# highered_access <- read_rds("app/data/highered_access.rds")
openspace_access <- read_rds("app/data/openspace_access.rds")
openspace_conservation_access<- read_rds("app/data/openspace_conservation_access.rds")
# openspace_paths_access <- read_rds("app/data/openspace_paths_access.rds")

dasy_raster<- read_rds("app/data/dasy_raster.rds")
weights_all_for_plot <- read_rds("app/data/weights_for_all_plot.rds")
comm_types_rast<- read_rds("app/data/comm_types_rast.rds")

  # "Boston Region MPO"= 8,
  # "Developing Suburbs: Country Suburbs"= 3,
  # "Developing Suburbs: Maturing New England Towns"= 1, 
  # "Inner Core: Metro Core Communities" = 6,
  # "Inner Core: Streetcar Suburbs"= 2,
  # "Maturing Suburbs:Established Suburbs and Cape Cod Towns"=4,
  # "Maturing Suburbs: Mature Suburban Towns"= 5,
  # "Regional Urban Centers: Sub-Regional Urban Centers" = 7


# write_stars(openspace_access, "GIS/test/open_space_access.iff" )

## Access to Parks ###
# 15 min walk to all parks
openspace_walk <- openspace_access %>% select(OpenSpace_Weekend_Walk_15min)
## For MPO
write_stars(openspace_walk, "GIS/test/openspace_walk_MPO.tiff")
## For Metro Core
metro_core <- comm_types_rast %>% select(id6)
openspace_walk_metro_core <- openspace_walk*metro_core
write_stars(openspace_walk_metro_core, "GIS/test/openspace_walk_metrocore.tiff")
## For Country Suburbs
country_sub <- comm_types_rast %>% select(id3)
openspace_walk_country_sub <- openspace_walk*country_sub
write_stars(openspace_walk_country_sub, "GIS/test/openspace_walk_country_sub.tiff")

# Transit access to large parks
largeParks_transit<- openspace_conservation_access %>% 
  select(OpenSpace_Conservation_Weekend_TransitAll_45min)
## For MPO
write_stars(largeParks_transit, "GIS/test/largeParks_transit_MPO.tif")
## For MetroCore
largeParks_transit_metro_core <-largeParks_transit*metro_core
## For StreetCar Suburbs
streetcar_sub <- comm_types_rast %>% select(id2)
largeParks_transit_streetcar <-largeParks_transit*streetcar_sub %>% 
  write_stars("GIS/test/largeParks_transit_streetcar.tif")


# Population density not a predictor to equitable accesss within community types
# pop density 
write_stars()
# low-income count. pct.
# minority count. pct.

# essential places
# country cuburbs
# new england towns