library(tidyverse)
library(sf)
library(stars)

# Model result outputs summarized here:
# https://www.ctps.org/data/pdf/plans/LRTP/destination/Destination-2040-LRTP-20191030.pdf#page=243

# SETUP INPUTS ####
boundary<-st_read("output/AggregationAreas.gpkg", layer= "MPO_Boundary") %>% 
  st_transform(3857) # needs to be psudo-mercator for raster operations
mpoBoundary <- boundary
comm_types <- st_read("output/AggregationAreas.gpkg", layer= "CommunityTypes") %>%
  st_transform(3857)

# Prep demographic data ####
prep_grid <- read_stars("data/lodes-data-2018 Workers total conveyal.tif") %>% 
   st_crop(boundary) #%>% 
#   st_as_sf() %>% 
#   rename(workers = 1)
# st_write(prep_grid, "notebooks/data/mpo_conveyal_grid_as_vector.gpkg", layer= 'workers')
names(prep_grid) = 'lodes_wkrs'

demo <- st_read("output/DemographicData.gpkg", layer = 'tracts_acs_dec_2020') %>% 
  st_transform(3857)
dasy_demo <- st_read("notebooks/pop_output/dasy_demo.gpkg", layer = 'interpolated') %>% 
  st_transform(st_crs(prep_grid))

dasy_raster <- dasy_demo %>% 
  st_rasterize(template = prep_grid)
# write_rds(dasy_raster, "app/data/dasy_raster.rds")

# Check demographic totals
# make sure that populations in census tract demo are account for in the dasymetric mapping process
demo_summary <- demo %>% 
  st_drop_geometry() %>% 
  distinct(GEOID, pop_dec, hh_dec,pop_dec_adult, minority, minority_adult, nonminority, nonminority_adult,
           lowinc, lowinc_adult, nonlowinc, nonlowinc_adult, zero_veh_hh, non_zero_veh_hh) %>% 
  ungroup() %>% 
  janitor::adorn_totals() %>% 
  filter(GEOID == "Total")
dasy_demo_summary <- dasy_demo %>% 
  st_drop_geometry() %>% 
  mutate(id = row_number()) %>%
  select(id, everything()) %>% 
  janitor::adorn_totals() %>% 
  filter(id == "Total")

demo_check <- demo_summary %>% bind_rows(dasy_demo_summary)

# Prep aggregation data ####
# rasterize commtypes
comm_types_id <- comm_types %>% 
  st_drop_geometry() %>% 
  distinct(communityType, subtype) %>% 
  mutate(id = row_number())

# write_rds(comm_types_id, "app/data/comm_types_id.rds")
comm_types_byid <- comm_types %>% 
  left_join(comm_types_id)
comm_types_rast<- comm_types_byid %>% 
  select(id) %>% 
  st_rasterize(template = prep_grid)
#unique(as.vector(comm_types_rast$id))

plot(comm_types_rast)
comm_types_rast <- comm_types_rast %>% 
  mutate(
    id1 = ifelse(id == 1, 1, NA),
    id2 = ifelse(id == 2, 1, NA),
    id3 = ifelse(id == 3, 1, NA),
    id4 = ifelse(id == 4, 1, NA),
    id5 = ifelse(id == 5, 1, NA),
    id6 = ifelse(id == 6, 1, NA),
    id7 = ifelse(id == 7, 1, NA),
    id = ifelse( is.na(id)==F, 1, NA)
  )
#plot(comm_types_rast %>% select(id7))

#write_rds(comm_types_rast, "app/data/comm_types_rast.rds")

# read in conveyal runs ####
datafolder <- "data/ConveyalRuns/Sept2019/"
filenames_all <- list.files(datafolder)

# Read in conveyal access raster files, project and crop to mpo boundary
dest_types <-  unique(word(filenames_all[endsWith(filenames_all, ".geotiff")], 1, sep = "_"))
for(i in 1: length(dest_types)){
  dest_name <- str_to_lower(dest_types[[i]])
  files <- filenames_all[endsWith(filenames_all, ".geotiff")]
  files <- files[grepl(dest_types[i], files)]
  files <- paste0(datafolder,files)
access_data <- read_stars(files, quiet= T)
names(access_data)<- str_remove(names(access_data),"_50pct.geotiff")
# note cropping pulls raster cells based on whether the cell center falls within the boundary
access_data <- suppressWarnings(st_crop(access_data, boundary))
# access_data[[1]][access_data[[1]] < 1] = NA

assign(dest_name, access_data)
}
 rm(dest_name, files, access_data)
# SPLIT out multiple runs for same destination types
healthcareEmg <- healthcare %>% select(starts_with("Healthcare_Emergency"))
healthcareNonEmg <- healthcare %>% select(starts_with("Healthcare_Nonemergency"))
rm(healthcare) 

openspace_conservation <- openspace %>% select(starts_with("OpenSpace_Conservation"))
openspace_paths <- openspace %>% select(starts_with("OpenSpacePaths_"))
openspace <- openspace %>% select(starts_with("OpenSpace_Weekend"))
rm(openspacepaths)

# write to app data
# write_rds(healthcareNonEmg, "app/data/healthcareNonEmg_access.rds")
# write_rds(healthcareEmg, "app/data/healthcareEmg_access.rds")
# write_rds(jobs, "app/data/jobs_access.rds")
# write_rds(essentialplaces, "app/data/essentialplaces_access.rds")
# write_rds(highered, "app/data/highered_access.rds")
# write_rds(openspace, "app/data/openspace_access.rds")
# write_rds(openspace_conservation, "app/data/openspace_conservation_access.rds")
# write_rds(openspace_paths, "app/data/openspace_paths_access.rds")

# Visualize by access type ####
visualize_for_access <- function(access){
  if (length(names(access))>1 ){
    access <- access %>% st_redimension()
    ggplot()+
      geom_stars(data = access)+
      geom_sf(data= mpoBoundary,size=.4,color="light gray", fill= 'transparent')+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt", #'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
      facet_wrap(~new_dim)+
      theme_void()#+
    #theme(text=element_text(family="Helvetica"))
  } else {
    ggplot()+
      geom_stars(data = access)+
      geom_sf(data=mpoBoundary,size=.4,color="light gray", fill= "transparent")+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",#trans= 'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
      theme_void()+
      theme(text=element_text(family="Helvetica"))
  }
}



jobs_access_vis <- visualize_for_access(jobs)
jobs_access_vis
highered_access_vis <- visualize_for_access(highered)
highered_access_vis
test <- highered*dasy_raster$pop_dec_adult
plot(test$HigherEd_MD_TransitAll_30min)
# weighted average region wide####

get_weighted_avgs_mpo <- function(access_layer, weights=dasy_raster){
  # access_layer <- highered$HigherEd_MD_TransitAll_30min
  access_vector <- as.vector(access_layer)
  weighted_mean_for_avg <- function(w){result <- round(weighted.mean(access_vector, as.vector(w), na.rm=T))}
  weighted_avgs <- lapply(weights, weighted_mean_for_avg) %>% 
    enframe() %>%
    mutate(
      pop = name,
      AvgAccessOpps = as.numeric(value)) %>% 
    rowwise() %>% 
    mutate(
      type = str_split(pop, "_")[[1]][1],
           type = case_when(
             grepl("min_adult", pop) ~ "Minority Status, adult",
             grepl("min", pop) ~ "Minority Status",
             grepl("inc_adult", pop) ~ "Income status, adult",
             grepl("inc", pop) ~ "Income status",
             grepl("veh_hh", pop) ~ "Household vehicles",
             grepl("pop_dec_adult", pop)~ "Total population, adult",
             grepl("pop_dec", pop) ~ "Total population",
             grepl("hh_dec", pop) ~ "Total households"),
           pop_name= case_when(
             pop == "minority_adult" ~ "Minority, adult",
             pop == "nonminority_adult" ~ "Nonminority, adult",
             pop == "minority" ~ "Minoirty",
             pop == "nonminority" ~ "Nonminority",
             pop == "lowinc" ~ "Low-income",
             pop == "nonlowinc" ~ "Non-low-income",
             pop == "lowinc_adult" ~ "Low-income, adult",
             pop == "nonlowinc_adult" ~ "Non-low-income, adult",
             pop == "zero_veh_hh" ~ "Zero vehicle households",
             pop == "non_zero_veh_hh" ~ "Non-zero vehicle households",
             grepl("pop_dec_adult", pop)~ "Total population, adult",
             grepl("pop_dec", pop) ~ "Total population",
             grepl("hh_dec", pop) ~ "Total households"))
  # TODO: develop ratio and percent outputs for comparisons
  
  
  return(weighted_avgs)
}

# # test function for one layer
# highered_TransitAll_30min <- get_weighted_avgs_mpo(highered$HigherEd_MD_TransitAll_30min, dasy_raster) %>% 
#   mutate(Region = "MPO")
# # setup apply statement for all layers
# test<- lapply(highered, get_weighted_avgs_mpo)
# test2 <- plyr::ldply(test, data.frame) %>% 
#   mutate(region = "MPO")
get_weighted_avgs <- function(access_layer, weights){
  # access_layer <- access[1]
  # weights <-  dasy_raster 
  # weights <- dasy_filtered
  access_vector <- as.vector(access_layer)
  weighted_mean_for_avg <- function(w){result <- weighted.mean(access_vector, as.vector(w), na.rm=T)}
  weighted_avgs <- lapply(weights, FUN = weighted_mean_for_avg) %>% 
    enframe() %>%
    mutate(
      pop = name,
      AvgAccessOpps = as.numeric(value)) %>% 
    rowwise() %>% 
    mutate(
      type = str_split(pop, "_")[[1]][1],
      type = case_when(
        grepl("minority_adult", pop) ~ "Minority Status, adult",
        grepl("min", pop) ~ "Minority Status",
        grepl("inc_adult", pop) ~ "Income status, adult",
        grepl("inc", pop) ~ "Income status",
        grepl("veh_hh", pop) ~ "Household vehicles",
        grepl("pop_dec_adult", pop)~ "Total population, adult",
        grepl("pop_dec", pop) ~ "Total population",
        grepl("hh_dec", pop) ~ "Total households"),
      pop_name= case_when(
        pop == "minority_adult" ~ "Minority, adult",
        pop == "nonminority_adult" ~ "Nonminority, adult",
        pop == "minority" ~ "Minoirty",
        pop == "nonminority" ~ "Nonminority",
        pop == "lowinc" ~ "Low-income",
        pop == "nonlowinc" ~ "Non-low-income",
        pop == "lowinc_adult" ~ "Low-income, adult",
        pop == "nonlowinc_adult" ~ "Non-low-income, adult",
        pop == "zero_veh_hh" ~ "Zero vehicle households",
        pop == "non_zero_veh_hh" ~ "Non-zero vehicle households",
        grepl("pop_dec_adult", pop)~ "Total population, adult",
        grepl("pop_dec", pop) ~ "Total population",
        grepl("hh_dec", pop) ~ "Total households"))
  # TODO: develop ratio and percent outputs for comparisons
  
  
  return(weighted_avgs)
}

# set up loop to get through all access layers
access_all <- list(healthcareNonEmg, healthcareEmg, jobs, essentialplaces, highered, openspace, openspace_paths, openspace_conservation)
comm_filters <- comm_types_id %>% 
  mutate(comm_filter = map(.x= id, ~select(comm_types_rast, paste0("id", .x))))

access_all_avgs <- tibble()

for (i in 1:length(access_all)){
  # i <- 3
  access <- access_all[i][[1]]
  avgs_mpo <- lapply(access, get_weighted_avgs, weights= dasy_raster)
  avgs_mpo <- plyr::ldply(avgs_mpo, data.frame) %>% 
    mutate(region = "MPO")
  
  access_avgs_agg <- tibble()
  for(j in 1: 7){
    # j <- 3
    agg <- j
    if (agg == 1) { comm_filter <- comm_types_rast %>% select(id1)}
    if (agg == 2) { comm_filter <- comm_types_rast %>% select(id2)}
    if (agg == 3) { comm_filter <- comm_types_rast %>% select(id3)}
    if (agg == 4) { comm_filter <- comm_types_rast %>% select(id4)}
    if (agg == 5) { comm_filter <- comm_types_rast %>% select(id5)}
    if (agg == 6) { comm_filter <- comm_types_rast %>% select(id6)}
    if (agg == 7) { comm_filter <- comm_types_rast %>% select(id7)}
    #if (agg == 8) { comm_filter <- comm_types_rast %>% select(id)}
    
    if (agg == 1) { access_agg <- access * (comm_types_rast %>% select(id1))}
    if (agg == 2) { access_agg <- access * (comm_types_rast %>% select(id2))}
    if (agg == 3) { access_agg <- access * (comm_types_rast %>% select(id3))}
    if (agg == 4) { access_agg <- access * (comm_types_rast %>% select(id4))}
    if (agg == 5) { access_agg <- access * (comm_types_rast %>% select(id5))}
    if (agg == 6) { access_agg <- access * (comm_types_rast %>% select(id6))}
    if (agg == 7) { access_agg <- access * (comm_types_rast %>% select(id7))}
    #if (agg == 8) { access_agg <- access * (comm_types_rast %>% select(id))}
    dasy_filtered <- dasy_raster * comm_filter
    
    avgs <- lapply(access_agg, get_weighted_avgs,weights= dasy_filtered)
    avgs <- plyr::ldply(avgs, data.frame) %>% 
      mutate(agg_id = as.numeric(agg)) %>%
      left_join(comm_types_id, by= c("agg_id" = "id")) %>% 
      mutate(region = paste0(communityType, ': ', subtype)) %>% 
      select(-c(agg_id, communityType, subtype))
    access_avgs_agg <- bind_rows(access_avgs_agg,
                                 avgs)
  }
  
  access_all_avgs <- bind_rows(access_all_avgs,
                               avgs_mpo, access_avgs_agg)
  rm(access_avgs_agg)
}





test <- tibble(
  dest_type = c("Healthcare, Non-emergency", "Healthcare, Emergency", 
                "Jobs", "Essential Places", "Higher Education", 
                "Open Space", "Open Space, Paths", "Open Space, Conservation")) %>% 
  mutate(access= access_all) %>% 
  full_join(comm_filters, by = character()) 
  

