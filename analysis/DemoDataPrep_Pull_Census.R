# MPO Region Demographics
# pull demo data for all of MA, then filter down to Boston Region MPO

# PACKAGES ####
library(tidyverse)
library(tidycensus)
library(mapview)

# FUNCTIONS ####
source("functions/census_demo_pull.R")
# Inputs ###
br_mpo_munis <- read_csv("data/town_codes.csv") %>% 
  filter(MPO == "Boston")

demo_data<- read_csv("data/demo_data_plan.csv") %>% 
  # TODO: in future work on vehicle access
  # TODO: determine if block groups are appropriate
  filter(demo %in% c("incstatus", "minstatus")) %>% 
  filter(geogs == "tract")


# TODO: either use a mutate if function so doesn't need to splinter and filter or make helper function in census data pull
minstatus_acs_dec <- demo_data %>% 
  filter(demo == "minstatus" & is.na(acs_tables) == F) %>% 
  mutate(demo_data = pmap(list(year_acs = y_acs, year_dec = y_dec, 
                               state = state, census_geog = geogs,
                               universe_type = universe_type), min_status_acs_dec))

minstatus_dec <- demo_data %>% 
  filter(demo == "minstatus"& is.na(acs_tables) == T) %>% 
  filter(y_dec == 2020) %>% 
  mutate(demo_data= pmap(list(year_dec = y_dec,
                              state = state, census_geog = geogs,
                              universe_type = universe_type), min_status_dec))

incstatus_acs_dec <- demo_data %>% 
  filter(demo == "incstatus" & type == "200% FPL") %>% 
  mutate(demo_data = pmap(list(year_acs = y_acs, year_dec = y_dec,
                               state = state, census_geog = geogs,
                               universe_type = universe_type), inc_status_FPL_acs_dec))
  

# Minority Population ####
minstatus_tract_dec <- min_status_by_tract_dec(year_dec, "MA")


# mapview::mapview(dec_minstatus_tract, zcol = 'minority_pct')
# 
# ggplot(dec_minstatus_tract)+
#   geom_sf(aes(fill = minority_pct, color = minority_pct))+
#   coord_sf(crs= 26986 )+
#   scale_fill_continuous(label = scales::percent)+
#   scale_color_continuous(label = scales::percent) 

# Low-income Population ####
# 
# 



# Population within the BRMPO ####
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
# st_write(br_mpo_equity_pop,"output/demo_brmpo_equity_pop.gpkg","equity_pop_2020")

# Minority Status BRMPO Summary
minstatus <- br_mpo_equity_pop %>% 
  st_drop_geometry() %>% 
  summarize(minority_pop= sum(minority_pop),
            nonminority_pop = sum(nonminority_pop),
            total_pop = sum(total_pop)) %>% 
  mutate(minority_pct = minority_pop/total_pop,
         nonminority_pct = nonminority_pop/total_pop)
