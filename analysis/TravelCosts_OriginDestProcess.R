library(tidyverse)
library(sf)
library(mapview)

# Travel time costs 
# Value of travel time (VTT) calculated 
# https://www.vtpi.org/tca/tca0502.pdf
# see table 17: Table 17 https://www.vtpi.org/traveltime.pdf

# read in conveyal outputs drive times and transit paths
drive_times <- read_csv("data/ConveyalRuns/TravelCosts_OD_Drive_AMPeak_TIMES.csv")
transit_paths <- read_csv("data/ConveyalRuns/TravelCosts_OD_Transit_AMPeak_PATHS.csv")
# filter to od pairs

pts<- st_read("output/TravelCosts.gpkg", layer = "ODpts")

drive_times <- drive_times %>% 
  filter(percentile == 50) %>% 
  left_join(st_drop_geometry(pts), by= c("origin" = "id")) %>% 
  filter(type == "o") %>% 
  rename(name_origin = name) %>%
  select(-type) %>% 
  left_join(st_drop_geometry(pts), by = c("destination"= "id")) %>% 
  filter(type == "d") %>% 
  rename(name_destination = name) %>% 
  select(-type) %>% 
  rename(time_in_vehicle = time) %>%
  # conveyal reports inaccessible drive times as -1
  # example is a origin that ended up in the ocean/on an island nd a destination on land
  filter(time_in_vehicle > 0)

# accessTime: Walking from the selected origin reaching transit
# egressTime: Traveling from transit stops to the point on the street network nearest to the destination
# rideTime: Riding in transit vehicles
# transferTime: time to get from alight to next boarding (if not same stop)
# waitTime: time waiting for transit to arrive

transit_paths <- transit_paths %>% 
  left_join(st_drop_geometry(pts), by= c("origin" = "id")) %>% 
  filter(type == "o") %>% 
  rename(name_origin = name) %>%
  select(-type) %>% 
  left_join(st_drop_geometry(pts), by = c("destination"= "id")) %>% 
  filter(type == "d") %>% 
  rename(name_destination = name) %>% 
  select(-type) %>% 
  filter(name_origin != name_destination) %>% 
  rowwise() %>% 
  mutate(time_walk_to_stop = accessTime,
         # note if just walking to destination is fastest then egress is NA but walk time is in totalTime
         time_walk_to_destination = ifelse(is.na(egressTime)==T, totalTime, egressTime),
         time_in_vehicle = sum(as.numeric(str_split(rideTimes,"\\|")[[1]])),
         time_wait_at_stop = sum(as.numeric(str_split(waitTimes, "\\|")[[1]])),
         # 10 minute penalty per transfer 
         # number of transfers found by counting pipes in the routes field
         transfer_penalty = (str_count(routes, "\\|")*10),
         time_transfer_wait = sum(as.numeric(str_split(transferTime, "\\|")[[1]]))) 
  # note if all transit times are NA  (or routes are NA) but total time is greater than zero, 
  # then total time represents the time to walk to the destination
  
# from 2020 5yr ACS data
# median earnings mpo $56,507.64 (table B08119)
# median hh inc mpo $97,085.11  (table B19001)
wage_rate_annual <- 56507.64
wage_rate<- wage_rate_annual/(40*52) # assuming 40hr work week and 52 weeks in a year


price_travel_transit<- function(
                        time_walk_to_stop,
                        time_wait_at_stop,
                        time_in_vehicle, 
                        transfer_penalty,
                        time_transfer_wait,
                        time_walk_to_destination,
                        wage_rate_hrly= wage_rate){
  cost <- sum(
    time_walk_to_stop/60*wage_rate_hrly*.7,
    time_wait_at_stop/60*wage_rate_hrly*.7,
    time_in_vehicle/60*wage_rate_hrly*.67,
    transfer_penalty/60*wage_rate_hrly*.35,
    time_transfer_wait/60*wage_rate_hrly*.7,
    time_walk_to_destination/60*wage_rate_hrly*.7, na.rm = T)
  return(cost)
}
price_travel_drive <- function(wage_rate_hrly= wage_rate, 
                               time_in_vehicle) {
  cost <- sum(
    time_in_vehicle/60*wage_rate_hrly*.67, na.rm = T)
  return(cost)
}


# Price travel, drive
drive_times_priced <- drive_times %>% 
  mutate(cost_drive = map_dbl(.x = time_in_vehicle, ~price_travel_drive(time_in_vehicle= .x)))
# Price travel, transit
transit_paths_priced <- transit_paths %>% 
  mutate(costTransit = pmap_dbl(list(time_walk_to_stop,
                              time_wait_at_stop,
                              time_in_vehicle, 
                              transfer_penalty,
                              time_transfer_wait,
                              time_walk_to_destination), price_travel_transit))
transit_costs <- transit_paths_priced %>% 
  select(origin, destination, name_origin, name_destination, totalTime, costTransit) %>% 
  group_by(origin, destination, name_origin, name_destination) %>% 
  summarise(min_time = min(totalTime),
            max_time = max(totalTime),
            mean_time = mean(totalTime),
            min_costTransit = min(costTransit),
            max_costTransit = max(costTransit),
            mean_costTransit = mean(costTransit),
            n_transit = n())


drive_costs <-  drive_times_priced %>% 
  select(origin, destination, name_origin, name_destination, drive_time = time_in_vehicle, cost_drive)

costs <- drive_costs %>% 
  left_join(transit_costs)

# costs with lines
pts_coord <- pts %>%
  rowwise() %>%
  mutate(lon = unlist(geom)[1],
         lat = unlist(geom)[2]) %>%
  select(name, lon, lat) %>%
  st_drop_geometry()

# make od lines
make_line <- function(lon1, lat1, lon2, lat2) {
  st_linestring(matrix(c(lon1, lon2, lat1, lat2), 2, 2))
}

cost_lines <- costs %>%
  left_join(pts_coord, by = c("name_origin" = "name")) %>%
  rename(lon1= lon, lat1= lat) %>%
  left_join(pts_coord, by = c("name_destination"= "name")) %>%
  rename(lon2= lon, lat2 = lat) %>%
  mutate(geometry = pmap(list(lon1, lat1, lon2,lat2), make_line)) %>%
  st_as_sf(crs= 4326) %>%
  select(-c(lon1, lon2, lat1, lat2))


tracts <- st_read("output/DemographicData.gpkg", "tracts_acs_dec_2020") %>% 
  st_transform(4326) %>% 
  select(GEOID, pop_dec)

cost_byTract <- costs %>%
  left_join(tracts, by = c("name_origin"= "GEOID")) %>% 
  filter(pop_dec > 0) %>% 
  st_as_sf() 
  
test <- cost_byTract %>% filter(name_destination == 1065)
mapview(test, zcol = 'mean_costTransit')+ filter(pts, name == '25009202104')

st_write(cost_lines,"output/TravelCosts.gpkg", layer = "cost_lines", append = F)
st_write(cost_byTract, "output/TravelCosts.gpkg", layer = "cost_byTract", append = F)
write_csv(costs, "output/travel_costs.csv")
