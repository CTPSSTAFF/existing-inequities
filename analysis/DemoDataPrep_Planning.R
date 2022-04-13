library(tidyverse)
library(tidycensus)
# TODO: Pull out zero vehicle households

# Demo groups
demo <- tibble(demo= c("minstatus", "incstatus"))
# Types
inc_types <- tibble(demo = c(rep("incstatus",2)), type= c("200% FPL", "60% AMI")) %>% 
  filter(type != "60% AMI")

total_pop_type <- tibble(universe_type = c("age 18 and older",
                                           "total population"))

# Decennial Years
year_acs <- tibble(y_acs = c(2019, 2020)) 
years<- year_acs %>% 
  mutate(y_dec= y_acs - y_acs%%10) %>% 
  add_row(y_dec = 2010) %>% 
  add_row(y_dec = 2020)

# Geographies
geogs <- tibble(geogs = c("tract", "block group"))
service_area <- tibble(service_area = c("MPO")) # "MBTA", "MBTA +"
state <- service_area %>% 
  mutate(state = case_when(
    service_area == "MPO" ~ "MA",
    service_area == "MBTA" ~ "MA, RI",
    service_area == "MBTA +" ~ "MA, RI, NH"))


demo_data <- demo %>% 
  left_join(inc_types) %>% 
  # full_join(geogs, by= character(),) %>%
  full_join(service_area, by= character()) %>% 
  left_join(state) %>% 
  full_join(years, by= character()) %>% 
  left_join(total_pop_type, by= character()) %>% 
  # income not reported in decennial
  filter(!(demo=="incstatus" & is.na(y_acs)==T)) %>% 
  mutate(acs_tables = case_when(
    demo== "minstatus" & is.na(y_acs)==F & universe_type == "age 18 and older" ~ "B01001H",
    demo== "minstatus" & is.na(y_acs)==F & universe_type == "total population" ~ "B03002",
    demo== "incstatus" & type == "200% FPL" & is.na(y_acs)==F & universe_type == "age 18 and older" ~ "B17024",
    demo== "incstatus" & type == "200% FPL" & is.na(y_acs)==F & universe_type == "total population" ~ "C17002",
    TRUE ~ NA_character_
  )) %>% 
  mutate(dec_tables = case_when(
    universe_type == "age 18 and older" ~ "P4_001N, P004001",
    universe_type == "total population" ~ "P2_001N, P002001",
    TRUE ~ NA_character_
  ))


# list tables needed to find data
acs20 <- load_variables(2020, "acs5", cache = T)

acs19 <- load_variables(2019, "acs5", cache = T)
# C17002_001 Estimate!!Total: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# C17002_008 Estimate!!Total:!!2.00 and over RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS

# B17024_001 Estimate!!Total:  AGE BY RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS

# B03002_001 Estimate!!Total:HISPANIC OR LATINO ORIGIN BY RACE
# B03002_003 Estimate!!Total:!!Not Hispanic or Latino:!!White alone HISPANIC OR LATINO ORIGIN BY RACE

# B01001H_001 Estimate!!Total:SEX BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)
# B01001H_003 Estimate!!Total:!!Male:!!Under 5 years
# B01001H_004 Estimate!!Total:!!Male:!!5 to 9 years
# B01001H_005 Estimate!!Total:!!Male:!!10 to 14 years
# B01001H_006 Estimate!!Total:!!Male:!!15 to 17 years
# B01001H_018 Estimate!!Total:!!Female:!!Under 5 years
# B01001H_019 Estimate!!Total:!!Female:!!5 to 9 years
# B01001H_020 Estimate!!Total:!!Female:!!10 to 14 years
# B01001H_021 Estimate!!Total:!!Female:!!15 to 17 years


dec20 <- load_variables(2020, "pl", cache = T)
# total population, min status
# P2_001N !!Total:
# P2_005N !!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone

# P4_001N !!Total:HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER
# P4_005N!!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER


dec10 <- load_variables(2010, "pl", cache = T)
# P002001 Total
# P002005 Total!!Not Hispanic or Latino!!Population of one race!!White alone

# P004001 Total HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER
# P004004 Total!!Not Hispanic or Latino!!Population of one race HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER



# 
# # need low income thresholds for 60% ami
# br_mpo_munis <- read_csv("data/town_codes.csv") %>% 
#   filter(MPO == "Boston")
# 
# br_mpo_60pct_ami <- get
