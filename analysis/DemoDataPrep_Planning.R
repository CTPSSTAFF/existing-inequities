library(tidyverse)
library(tidycensus)

# Demo groups
demo <- tibble(demo= c("minstatus", "incstatus"))
# Types
inc_types <- tibble(demo = c(rep("incstatus",2)), type= c("200% FPL", "60% AMI"))

total_pop_type <- tibble(universe_type = c("aged 16 and older",
                                           "aged 18 and older",
                                           "total population"))

# Decennial Years
year_acs <- tibble(y_acs = c(2019, 2020)) 
years<- year_acs %>% 
  mutate(y_dec= y_acs - y_acs%%10)

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
  full_join(geogs, by= character(),) %>%
  full_join(service_area, by= character()) %>% 
  left_join(state) %>% 
  full_join(years, by= character()) %>% 
  left_join(total_pop_type, by= character())


# list tables needed to find data
acs20 <- load_variables(2020, "acs5", cache = T)
acs19 <- load_variables(2019, "acs5", cache = T)
  
dec20 <- load_variables(2020, "pl", cache = T)
# P2_001N !!Total:
# P2_005N !!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone

dec10 <- load_variables(2010, "pl", cache = T)
# P001001 Total Population
# P001003 Total Population White alone
# P002001 Total
# P002005 Total!!Not Hispanic or Latino!!Population of one race!!White alone


# need low income thresholds

