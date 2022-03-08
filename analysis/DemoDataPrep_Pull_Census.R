# MPO Region Demographics
# pull demo data for all of MA, then filter down to Boston Region MPO

# PACKAGES ####
library(tidyverse)
library(tidycensus)
library(mapview)

# FUNCTIONS ####
source("functions/census_demo_pull.R")

# Inputs ###
year_acs <- 2019 # TODO: update to 2020 once 2016-20 data is vetted
year_dec <- 2020
state <- "MA"
br_mpo_munis <- read_csv("data/town_codes.csv") %>% 
  filter(MPO == "Boston")

# Minority Population ####
minstatus_tract_dec <- min_status_by_tract_dec(year_dec, "MA")
# TODO: clean up tracts

# mapview::mapview(dec_minstatus_tract, zcol = 'minority_pct')
# 
# ggplot(dec_minstatus_tract)+
#   geom_sf(aes(fill = minority_pct, color = minority_pct))+
#   coord_sf(crs= 26986 )+
#   scale_fill_continuous(label = scales::percent)+
#   scale_color_continuous(label = scales::percent) 



# Population with in the BRMPO ####
ma_muni_geog <- get_decennial(geography= "county subdivision", 
                              variables = "P1_001N",
                              year = year_dec,
                              state = state,
                              geometry = T)
br_mpo_geog <- ma_muni_geog %>% 
  filter(GEOID %in% br_mpo_munis$GEOID)

mapview(br_mpo_geog)+ mapview(minstatus_tract_dec) +mapview(br_mpo_equity_pop, col.region = "blue")

br_mpo_equity_pop <- minstatus_tract_dec %>% 
 st_intersection(select(br_mpo_geog, Subcounty= NAME, GEOID_Subcounty= GEOID))
minsta