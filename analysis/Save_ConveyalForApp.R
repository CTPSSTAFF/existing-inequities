library(tidyverse)
library(stars)

# input options
app_inputs <- tibble(dest = 
                 c(rep("Essential Places", 2),rep("Healthcare, emergency", 2), rep("Healthcare, non-emergency", 2),
                   rep("Higher Education", 2),rep("Jobs", 2),rep("Parks, All", 2),rep("Parks, Large", 2),rep("Paths", 2)), 
               modes =  c("Bike", "Walk","Drive", "Transit", "Drive", "Transit","Drive", "Transit",
                   "Drive", "Transit","Bike", "Walk","Drive", "Transit","Bike", "Walk"), 
               times = c(15, 15, 45, 45,45, 45,45, 45,45, 45, 15, 15, 45, 45,15, 15)) %>% 
  mutate(mode_time = paste(times, "minute", modes)) %>% 
  mutate(dest_id= case_when(
    dest == "Essential Places" ~ 5,
    dest == "Healthcare, emergency" ~ 2,
    dest == "Healthcare, non-emergency" ~ 1,
    dest == "Higher Education" ~ 4,
    dest == "Jobs" ~ 3,
    dest == "Parks, All"~6,
    dest == "Parks, Large" ~ 7,
    dest == "Paths" ~ 8),
    mode_id = case_when(
      modes == "Bike" ~ 2,
      modes == "Drive" ~ 5,
      modes == "Transit" ~ 4,
      modes == "Walk" ~ 1),
    time_id = case_when(
      times == 15 ~ 1,
      times == 30 ~ 2, 
      times == 45 ~ 3)) %>% 
  group_by(dest) %>% 
  mutate(mt_id = row_number())
write_rds(app_inputs, "app/data/app_inputs.rds")

# read in processed rasters as stars objects
healthcareNonEmg<- read_rds("data/ConveyalRuns/Sept2019_Processed/healthcareNonEmg_access.rds")
healthcareEmg<- read_rds("data/ConveyalRuns/Sept2019_Processed/healthcareEmg_access.rds")
jobs <- read_rds("data/ConveyalRuns/Sept2019_Processed/jobs_access.rds")
essentialplaces<- read_rds("data/ConveyalRuns/Sept2019_Processed/essentialplaces_access.rds")
highered <- read_rds( "data/ConveyalRuns/Sept2019_Processed/highered_access.rds")
openspace<- read_rds("data/ConveyalRuns/Sept2019_Processed/openspace_access.rds")
openspace_conservation <- read_rds( "data/ConveyalRuns/Sept2019_Processed/openspace_conservation_access.rds")
openspace_paths<- read_rds( "data/ConveyalRuns/Sept2019_Processed/openspace_paths_access.rds")

# filter down to options included in app inputs
healthcareNonEmg <- healthcareNonEmg %>% select(ends_with("Drive_45min"), ends_with("TransitAll_45min"))
healthcareEmg <- healthcareEmg %>% select(ends_with("Drive_45min"), ends_with("TransitAll_45min"))
jobs <- jobs %>% select(ends_with("Drive_45min"), ends_with("TransitAll_45min"))
essentialplaces <- essentialplaces %>% select(ends_with("Walk_15min"), ends_with("Bike_15min"))
highered <- highered %>% select(ends_with("Drive_45min"), ends_with("TransitAll_45min"))
openspace <- openspace %>% select(ends_with("Walk_15min"), ends_with("Bike_15min"))
openspace_conservation <- openspace_conservation %>% select(ends_with("Drive_45min"), ends_with("TransitAll_45min"))
openspace_paths <- openspace_paths %>%  select(ends_with("Walk_15min"), ends_with("Bike_15min"))
# save to app data folder
write_rds(healthcareNonEmg, "app/data/healthcareNonEmg_access.rds")
write_rds(healthcareEmg, "app/data/healthcareEmg_access.rds")
write_rds(jobs, "app/data/jobs_access.rds")
write_rds(essentialplaces, "app/data/essentialplaces_access.rds")
write_rds(highered, "app/data/highered_access.rds")
write_rds(openspace, "app/data/openspace_access.rds")
write_rds(openspace_conservation, "app/data/openspace_conservation_access.rds")
write_rds(openspace_paths, "app/data/openspace_paths_access.rds")

    
# Save ratio outputs####
access_all_comp <- read_csv("output/access_all_comp.csv")
access_ratios_for_app <- access_all_comp %>% 
  mutate(app = case_when(
    destination == "Healthcare, non-emergency" & 
      time == 45 & 
      mode %in% c("Drive", "Transit (All modes)") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Healthcare, emergency" &
      time == 45 &
      mode %in% c("Drive", "Transit (All modes)") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Jobs" &
      time == 45 & 
      mode %in% c("Drive", "Transit (All modes)") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Essential Places" &
      time == 15 &
      mode %in% c("Walk", "Bike") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Higher Education" &
      time == 45 &
      mode %in% c("Drive", "Transit (All modes)") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Open Space, all parks" &
      time == 15 &
      mode %in% c("Walk", "Bike") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Open Space, paths" &
      time == 15 &
      mode %in% c("Walk", "Bike") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    destination == "Open Space, large parks" &
      time == 45 &
      mode %in% c("Drive", "Transit (All modes)") &
      type %in% c("Total population", "Minority status", "Income status", "Household vehicles") ~ T,
    TRUE ~ F
  ))%>% 
  mutate(app = ifelse(mode == "Drive" & type == "Household vehicles", F, app)) %>% 
  filter(app == T) %>% 
  #arrange(factor(Reg, levels = LETTERS[c(3, 1, 2)]), desc(Res), desc(Pop))
  arrange(destination, time, factor(region, levels = c("MPO" ,"Developing Suburbs: Country Suburbs",
                                                       "Developing Suburbs: Maturing New England Towns",
                                                       "Inner Core: Metro Core Communities" ,
                                                       "Inner Core: Streetcar Suburbs",
                                                       "Maturing Suburbs: Established Suburbs and Cape Cod Towns",
                                                       "Maturing Suburbs: Mature Suburban Towns" ,
                                                       "Regional Urban Centers: Sub-Regional Urban Centers"  
  ))) %>% 
  select(-app)
write_csv(access_ratios_for_app, "app/data/access_ratios.csv")
