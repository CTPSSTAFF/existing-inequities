# GOAL ####
# This script prepares the aggregation geographies for the region.
# Outputs are:
# 1. boundary layer: a boundary of the Boston Region MPO region
# 2. community_types layer: shapefiles of community types within the subregion
# 3. housing_submarket layer: shapefiles of the housing submarkets as defined by MAPC


# PACKAGES ####
library(tidyverse)
library(tidycensus)
library(sf)

# INPUTS ####
# BR MPO Boundary
br_mpo_munis <- read_csv("data/town_codes.csv") %>% 
  filter(MPO == "Boston")

# MPO Subregions
# Pulled from MPO website:https://www.ctps.org/mpo_communities#subregion
mpo_subregions <- read_csv("data/mpo_subcom.csv")

# MAPC Community Types
# pulled from http://www.mapc.org/wp-content/uploads/2017/09/Massachusetts-Community-Types-Summary-July_2008.pdf
mapc_commtypes <- read_csv("data/communityTypesMA.csv") %>% 
  mutate(municipality = ifelse(municipality == 'Manchester', 'Manchester-by-the-Sea', municipality))

# MAPC Housing Sub-Markets
# recommended by Tim Reardon
# https://housing-submarkets.mapc.org/
# https://datacommon.mapc.org/browser/datasets/422
mapc_housing <- read_csv("data/tabular.hous_submarkets_ct.csv") %>% 
  mutate(GEOID = as.character(ct10_id)) %>% 
  left_join(read_csv("data/housing-submarkets-desc.csv"))
# mapc_housing_meta <- read_csv("data/Housing Submarkets (Census Tracts)-metadata.csv")

# Pull the geography for the BRMPO ####
# From the 2020 Census pull the municipal level geogs
ma_muni_geog <- get_decennial(geography= "county subdivision", 
                              variables = "P1_001N",
                              year = 2020,
                              state = "MA",
                              geometry = T)
br_mpo_geog <- ma_muni_geog %>% 
  # pull out BRMPO from whole state by matching GEOIDs from town_codes table
  filter(GEOID %in% br_mpo_munis$GEOID) %>% 
  # pull muni name to easily joing to sub-regions later
  rowwise() %>% 
  mutate(municipality= str_split(NAME, " ")[[1]][1]) %>% 
  mutate(municipality = ifelse(municipality== "North", "North Reading", municipality))
# # check that town names pulled from census match municipality names from sub-region list
# all(br_mpo_geog$municipality %in% mpo_subregions$municipality)
# br_mpo_geog$municipality[br_mpo_geog$municipality %notin% mpo_subregions$municipality]

boundary <- br_mpo_geog %>% st_union(by_feature= F) %>% 
  st_as_sf() %>% 
  mutate(id = "Boston Region MPO") %>% 
  select(id, geometry= x) %>% 
  st_transform(26986)

sub_regions <- mpo_subregions %>% 
  left_join(br_mpo_geog) %>% 
  st_as_sf() %>% 
  group_by(subregion) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_transform(26986)

community_types <- br_mpo_geog %>% 
  left_join(mapc_commtypes)


# the MAPC housing subregions use 2010 census tracts
ma_tract10_geog <- get_decennial(geography= "tract", 
                              variables = "P002001",
                              year = 2010,
                              state = "MA",
                              geometry = T)
submarkets_by_tract <- ma_tract10_geog %>% 
  filter(GEOID %in% mapc_housing$ct10_id) %>%
  left_join(select(mapc_housing, GEOID, ct10_id, submkt_id, submarket, muni)) %>% 
  st_as_sf() %>% 
  select(GEOID, submkt_id,submarket, muni ) %>% 
  st_transform(26986)

submarkets <- submarkets_by_tract %>% 
  group_by(submkt_id, submarket) %>% 
  summarize(geometry = st_union(geometry))
  

# Check with visualization #### 
library(mapview)

mapview(boundary)+ sub_regions + 
  mapview(submarkets, zcol = 'submarket')+ 
  mapview(community_types, zcol='communityType')

# SAVE DATA ####
write_rds(boundary, "data/boundary.rds")

# Save as GeoPackage
st_write(boundary, "output/AggregationAreas.gpkg", "MPO_Boundary")
st_write(sub_regions, "output/AggregationAreas.gpkg", "MPO_SubRegions", append = T)
st_write(community_types, "output/AggregationAreas.gpkg", "CommunityTypes", append = T)
st_write(submarkets, "output/AggregationAreas.gpkg", "HousingSubmarkets", append = T)



# make community type descriptions and icons ####

comm_types<- unique(paste0(community_types$communityType, ": ", community_types$subtype))
ids <- read_rds( "app/data/comm_types_id.rds")


for (i in 1:length(comm_types)){
  typ <- word(comm_types[i], 1,sep = ": ")
  sub <- word(comm_types[i], 2, sep = ": ")
  
  id <- ids[ids$communityType==typ & ids$subtype==sub,]$id[[1]]
  munis <- community_types %>% 
    filter(communityType== typ & subtype== sub)
  
  plot <- ggplot()+
    geom_sf(data = community_types, fill = "transparent", color= 'light gray', size = .1)+
    geom_sf(data = boundary, size = .5, color = "light gray", fill = 'transparent')+
    geom_sf(data = munis, fill = "black")+
    coord_sf()+
    theme_void()
  ggsave(paste0("app/www/ct", id, ".png"), plot, width= 85, height = 85, units = "px", dpi = "screen", bg = "white")
  
}

no_hull <- community_types %>% 
  filter(municipality != "Hull")
mpo_plot <- ggplot()+
  geom_sf(data = community_types, fill = "black", color= 'transparent', size = .09)+
  geom_sf(data = no_hull , fill = "transparent", color = "light gray", size = .1)+
  # geom_sf(data = boundary, size = .5, color = "light gray", fill = 'transparent')+
  coord_sf()+
  theme_void()
mpo_plot
ggsave(paste0("app/www/ct", 8, ".png"), mpo_plot, width= 85, height = 85, units = "px", dpi = "screen", bg = "white")

