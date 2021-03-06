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
  filter(geogs == "tract")

# Pull Demo Data for all of MA using functions ####
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
veh_acs_dec <- demo_data %>% 
  filter(demo == 'vehicleaccess' & universe_type == "occupied households") %>% 
  mutate(demo_data = pmap(list(year_acs = y_acs, year_dec = y_dec,
                               state = state, census_geog = geogs,
                               universe_type = universe_type), no_vehicle_hh_acs_dec))


demo_data_ma <- bind_rows(minstatus_acs_dec,minstatus_dec,incstatus_acs_dec, veh_acs_dec)  
# write_rds(demo_data_ma, "data/demo_data_ma.rds")
demo_data_ma<- demo_data_ma %>% 
  filter(y_acs==2020)

# Pull Census Geog ###
ma_muni_geog <- get_decennial(geography= "county subdivision", 
                              variables = "P1_001N",
                              year = 2020,
                              state = "MA",
                              geometry = T)
br_mpo_geog <- ma_muni_geog %>% 
  filter(GEOID %in% br_mpo_munis$GEOID)
# mapview(br_mpo_geog)

# 2020 census tracts in the MPO
mpo_tract_geog<- get_acs(geography = "tract",
                        variable = "B03002_001",
                        year= 2020,
                        state = "MA",
                        geometry = T)%>% 
  st_intersection(st_geometry(br_mpo_geog)) %>% 
  select(GEOID, NAME)


# Join 2020 demo data with geog
mpo_tract_geog <- mpo_tract_geog %>% 
  left_join(select(demo_data_ma$demo_data[[1]], pop_dec_adult= pop_dec, everything())) %>% 
  left_join(demo_data_ma$demo_data[[2]]) %>% 
  left_join(select(demo_data_ma$demo_data[[3]], pop_dec_adult = pop_dec, percent_lowinc_adult = percent_lowinc,
                   percent_lowinc_moe_adult = percent_lowinc_moe, percent_nonlowinc_adult= percent_nonlowinc,
                   percent_nonlowinc_moe_adult= percent_nonlowinc_moe, everything())) %>% 
  left_join(demo_data_ma$demo_data[[4]]) %>% 
  left_join(demo_data_ma$demo_data[[5]]) %>% 
  select(GEOID, NAME, starts_with("pop_dec"),starts_with("hh_dec"), starts_with("percent_"), everything())
st_write(mpo_tract_geog, "output/DemographicData.gpkg","tracts_acs_dec_2020", driver= "GPKG")

#mpo_tract_geog<- st_read("output/DemographicData.gpkg", layer= "tracts_acs_dec_2020")


median_earnings_mpo <- get_median_inc(2020, "MA", br_mpo_geog, "earnings-worker")
# median earnings mpo $56,507.64
median_incHH_mpo <- get_median_inc(2020, "MA", br_mpo_geog, "income-household")
# median hh inc mpo $97,085.11
