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
# Are these essentially the same as MAPC Community types?
mpo_subregions <- read_csv("data/mpo_subcom.csv")

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
  select(id, geometry= x)

sub_regions <- mpo_subregions %>% 
  left_join(br_mpo_geog) %>% 
  st_as_sf() %>% 
  group_by(subregion) %>% 
  summarize(geometry = st_union(geometry))

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
  select(GEOID, submkt_id,submarket, muni )

submarkets <- submarkets_by_tract %>% 
  group_by(submkt_id, submarket) %>% 
  summarize(geometry = st_union(geometry))
  

# Check with visualization #### 
library(mapview)

mapview(boundary)+ sub_regions + mapview(submarkets, zcol = 'submarket')

# SAVE DATA ####
write_rds(boundary, "data/boundary.rds")

# Save as GeoPackage
st_write(boundary, "AggregationAreas.gpkg", "MPO_Boundary")
st_write(sub_regions, "AggregationAreas.gpkg", "CommunityTypes", append = T)
st_write(submarkets, "AggregationAreas.gpkg", "HousingSubmarkets", append = T)
