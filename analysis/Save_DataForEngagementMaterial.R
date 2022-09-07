# Engagement material map data prep
library(sf)
library(mapview)

# folder to write to
folder <- "output/EngagementMaterial_Data/"

# BASE LAYERS ####
mpo_boundary <- st_read("data/boundary.shp")

comm_types_id <- read_rds("app/data/comm_types_id.rds")
commTypes_byMuni <- st_read("app/data/AggregationAreas.gpkg", "CommunityTypes") %>% st_transform(26986)
commTypes<- commTypes_byMuni %>%
  group_by(communityType, subtype) %>%
  summarize(geometry = st_union(geom)) %>% 
  st_as_sf() %>% 
  left_join(comm_types_id) 

mapview(commTypes_byMuni)+ commTypes + mpo_boundary

st_write(mpo_boundary, paste0(folder, "mpo_boundary.shp"))
st_write(commTypes_byMuni, paste0(folder, "commTypes_byMuni.shp"))
st_write(commTypes, paste0(folder, "commTypes.shp"))

# DESTINATION DATA ####
# save geopackage layers as shapefiles for work in ArcGIS
parks_all_poly <- st_read("output/DestinationData.gpkg", layer = "OpenSpaceAccess_Poly") %>% 
  st_write(paste0(folder,"parks_all_poly.shp"))
parks_all_pt <- st_read("output/DestinationData.gpkg", layer = "OpenSpaceAccess_PT") %>% 
  st_write(paste0(folder, "parks_all_pt.shp"))

parks_large_poly <- st_read("output/DestinationData.gpkg", layer = "OpenSpaceAccess_large_POLY") %>% 
  st_write(paste0(folder, "parks_large_poly.shp"))
parks_large_pt <- st_read("output/DestinationData.gpkg", layer = "OpenSpaceAccess_large_PT")%>% 
  st_write(paste0(folder, "parks_large_pt.shp"))

essentialPlace_poly <- st_read("output/DestinationData.gpkg", layer = "essentialPlace_Final_POLY") %>% 
  st_write(paste0(folder, "essentialPlace_poly.shp"))
essentialPlace_pt <- st_read("output/DestinationData.gpkg", layer = "essentialPlace_Final_PT")%>% 
  st_write(paste0(folder, "essentialPlace_pt.shp"))




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


# write_stars(openspace_access, paste0(folder, "open_space_access.tiff" ))

## Access to Parks ###
# 15 min walk to all parks
openspace_walk <- openspace_access %>% select(OpenSpace_Weekend_Walk_15min)
## For MPO
write_stars(openspace_walk, paste0(folder, "openspace_walk_MPO.tif"))
## For Metro Core
metro_core <- comm_types_rast %>% select(id6)
openspace_walk_metro_core <- openspace_walk*metro_core
write_stars(openspace_walk_metro_core, paste0(folder, "openspace_walk_metrocore.tif"))
## For Country Suburbs
country_sub <- comm_types_rast %>% select(id3)
openspace_walk_country_sub <- openspace_walk*country_sub
write_stars(openspace_walk_country_sub, paste0(folder, "openspace_walk_country_sub.tif"))

# Transit access to large parks
largeParks_transit<- openspace_conservation_access %>% 
  select(OpenSpace_Conservation_Weekend_TransitAll_45min)
## For MPO
write_stars(largeParks_transit, paste0(folder, "largeParks_transit_MPO.tif"))
## For MetroCore
largeParks_transit_metro_core <-largeParks_transit*metro_core
## For StreetCar Suburbs
streetcar_sub <- comm_types_rast %>% select(id2)
largeParks_transit_streetcar <-largeParks_transit*streetcar_sub %>% 
  write_stars(paste0(folder, "largeParks_transit_streetcar.tif"))


# Population density not a predictor to equitable access within community types
# pop density 
pop_desnity <- dasy_raster %>% select(pop_dec) %>% 
  write_stars(paste0(folder, "total_population_2020.tif"))
# low-income pop
lowinc <- dasy_raster %>% select(lowinc) %>% 
  write_stars(paste0(folder, "lowinc_pop_2020.tif"))
# minority pop
minority <- dasy_raster %>% select(minority) %>% 
  write_stars(paste0(folder, "min_pop_2020.tif"))

# essential places
essentialplaces_walk <- essentialplaces_access %>% select(EssentialPlaces_AM_Walk_15min) %>% 
  write_stars(paste0(folder,"essentialPlace_walk_MPO.tif"))

# country suburbs
essentialplaces_walk_country_suburbs <- essentialplaces_walk*country_sub
write_stars(essentialplaces_walk_country_suburbs, paste0(folder, "essentialPlace_walk_country_sub.tif"))

# New England towns
neTowns <- comm_types_rast %>% select(id1)
essentialplaces_walk_netowns <- essentialplaces_walk*neTowns
write_stars(essentialplaces_walk_netowns, paste0(folder, "essentialPlace_walk_netowns.tif"))
