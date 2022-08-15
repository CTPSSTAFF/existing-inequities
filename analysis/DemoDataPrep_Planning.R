library(tidyverse)
library(tidycensus)

# Demo groups
demo <- tibble(demo= c("minstatus", "incstatus", "vehicleaccess"))
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
  left_join(geogs, by = character()) %>% 
  full_join(years, by= character()) %>% 
  left_join(total_pop_type, by= character()) %>% 
  # income not reported in decennial
  filter(!(demo=="incstatus" & is.na(y_acs)==T)) %>% 
  # vehicle access not reported in decennial
  filter(!(demo== "vehicleaccess" & is.na(y_acs)==T)) %>% 
  # for vehicle access pull occupied households
  mutate(universe_type = ifelse(demo== "vehicleaccess", "occupied households", universe_type)) %>% 
  distinct() %>% 
  mutate(acs_tables = case_when(
    demo== "minstatus" & is.na(y_acs)==F & universe_type == "age 18 and older" ~ "B01001H",
    demo== "minstatus" & is.na(y_acs)==F & universe_type == "total population" ~ "B03002",
    demo== "incstatus" & type == "200% FPL" & is.na(y_acs)==F & universe_type == "age 18 and older" ~ "B17024",
    demo== "incstatus" & type == "200% FPL" & is.na(y_acs)==F & universe_type == "total population" ~ "C17002",
    demo== 'vehicleaccess' & universe_type == 'occupied households' ~ "B08201",
    TRUE ~ NA_character_
  )) %>% 
  mutate(dec_tables = case_when(
    universe_type == "age 18 and older" ~ "P4_001N, P004001",
    universe_type == "total population" ~ "P2_001N, P002001",
    universe_type == "occupied households" ~ "H1_002N, H003002",
    TRUE ~ NA_character_
  ))

write_csv(demo_data, "data/demo_data_plan.csv")


# list tables needed to find data
acs20 <- load_variables(2020, "acs5", cache = T)
earnings <- acs20 %>% filter(grepl("EARNINGS IN THE PAST 12 MONTHS", concept))
unique(earnings$concept)
acs19 <- load_variables(2019, "acs5", cache = T)
dec20 <- load_variables(2020, "pl", cache = T)
dec10 <- load_variables(2010, "sf1", cache = T)
