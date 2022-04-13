# BR MPO Boundary
library(tidyverse)
library(tidycensus)
library(sf)

br_mpo_munis <- read_csv("data/town_codes.csv") %>% 
  filter(MPO == "Boston")
# Population with in the BRMPO ####
ma_muni_geog <- get_decennial(geography= "county subdivision", 
                              variables = "P1_001N",
                              year = 2020,
                              state = "MA",
                              geometry = T)
br_mpo_geog <- ma_muni_geog %>% 
  filter(GEOID %in% br_mpo_munis$GEOID)

boundary <- br_mpo_geog %>% st_union(by_feature= F) %>% 
  st_as_sf() %>% 
  mutate(id = "Boston Region MPO") %>% 
  select(id, geometry= x)
write_rds(boundary, "data/boundary.rds")



library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)

# test<- get_decennial(geography = "block group",  
#                      state = "MA",
#                      county = "Essex",
#                      variables = "P2_005N",
#                      summary_var = "P2_001N",
#                      year = 2020,
#                      geometry = TRUE,
#                      cb = FALSE) %>% 
#   mutate(value_min = summary_value - value,
#          pct_min = value_min / summary_value) %>% 
#   st_transform(26986) %>% 
#   tigris::erase_water(area_threshold = 0.95, year = 2020)
