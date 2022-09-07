library(tidyverse)
library(sf)
library(stars)

# SETUP INPUTS ####
## AGGREGATION AREAS INPUTS ####
boundary<-st_read("output/AggregationAreas.gpkg", layer= "MPO_Boundary") %>% 
  st_transform(3857) # needs to be psudo-mercator for raster operations?
comm_types <- st_read("output/AggregationAreas.gpkg", layer= "CommunityTypes") %>%
  st_transform(3857)

## DEOMGRAPHIC DATA ####
### Prep to rasterize demo ####
# pulling in Conveyal grid output to use the raster grid as a template to map demo data onto
prep_grid <- read_stars("data/lodes-data-2018 Workers total conveyal.tif") %>% 
   st_crop(boundary) #%>% 
#   st_as_sf() %>% 
#   rename(workers = 1)
# write template grid to python notebook data forlder to use the dasymetric mapping
# st_write(prep_grid, "notebooks/data/mpo_conveyal_grid_as_vector.gpkg", layer= 'workers')
names(prep_grid) = 'lodes_wkrs'


# read in census demo
demo <- st_read("output/DemographicData.gpkg", layer = 'tracts_acs_dec_2020') %>% 
  st_transform(3857)
# read in dasymetric output. Demo as raster from python notebook output
# note result is a vector of small tiles that needs to be rasterized
dasy_demo <- st_read("notebooks/pop_output/dasy_demo.gpkg", layer = 'interpolated') %>% 
  st_transform(st_crs(prep_grid))
dasy_raster <- dasy_demo %>% 
  st_rasterize(template = prep_grid)
# write_rds(dasy_raster, "app/data/dasy_raster.rds")

### Check demographic totals ####
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

# prep demo weights to use in app
total_pop_dec <- sum(as.vector(dasy_raster$pop_dec), na.rm = T)
weights_all_for_plot <- dasy_raster %>% 
  mutate(pct_min = minority/(minority+nonminority),
         pct_nonmin = nonminority/(minority+nonminority),
         pct_lowinc = lowinc/ (lowinc+ nonlowinc),
         pct_nonlowinc = nonlowinc/ (lowinc +nonlowinc),
         pct_zvhh = zero_veh_hh/(zero_veh_hh+ non_zero_veh_hh),
         pct_nonzvhh = non_zero_veh_hh/(zero_veh_hh+ non_zero_veh_hh),
         pct_pop = pop_dec/total_pop_dec
  ) %>% 
  select(starts_with("pct"), pop_dec,hh_dec, minority, nonminority, lowinc, nonlowinc, zero_veh_hh, non_zero_veh_hh)
write_rds(weights_all_for_plot, "app/data/weights_for_all_plot.rds")

## Prep aggregation data ####
### Rasterize commtypes ####
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

write_rds(comm_types_rast, "app/data/comm_types_rast.rds")

## READ IN CONVEYAL RUNS ####
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
access_data <- suppressWarnings(st_crop(access_data, boundary)) %>% st_normalize()
assign(dest_name, access_data)
}
rm(dest_name, files, access_data)
 
## SPLIT out multiple runs for same destination types ####
healthcareEmg <- healthcare %>% select(starts_with("Healthcare_Emergency"))
healthcareNonEmg <- healthcare %>% select(starts_with("Healthcare_Nonemergency"))
rm(healthcare) 

openspace_conservation <- openspace %>% select(starts_with("OpenSpace_Conservation"))
openspace_paths <- openspace %>% select(starts_with("OpenSpacePaths_"))
openspace <- openspace %>% select(starts_with("OpenSpace_Weekend"))
rm(openspacepaths)

### Save grouped rasters as stars objects #####
write_rds(healthcareNonEmg, "data/ConveyalRuns/Sept2019_Processed/healthcareNonEmg_access.rds")
write_rds(healthcareEmg, "data/ConveyalRuns/Sept2019_Processed/healthcareEmg_access.rds")
write_rds(jobs, "data/ConveyalRuns/Sept2019_Processed/jobs_access.rds")
write_rds(essentialplaces, "data/ConveyalRuns/Sept2019_Processed/essentialplaces_access.rds")
write_rds(highered, "data/ConveyalRuns/Sept2019_Processed/highered_access.rds")
write_rds(openspace, "data/ConveyalRuns/Sept2019_Processed/openspace_access.rds")
write_rds(openspace_conservation, "data/ConveyalRuns/Sept2019_Processed/openspace_conservation_access.rds")
write_rds(openspace_paths, "data/ConveyalRuns/Sept2019_Processed/openspace_paths_access.rds")

# Visualize by access type ####
# set up versions to match app variables so visualization functions match
commTypes_byMuni<- comm_types
mpoBoundary <- boundary 
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

library(ggiraph)
visualize_for_access2 <- function(access){
  if (length(names(access))>1 ){
    #access <- jobs %>% select(contains("30min"))
    access <- access %>% st_redimension()
    g<- ggplot()+
      geom_stars(data = access)+
      geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
      geom_sf_interactive(data = commTypes_byMuni, size=.2,
                          color = 'light gray',
                          fill = 'white', alpha= .001,
                          aes(tooltip = municipality, data_id = municipality))+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt", #'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      facet_wrap(~new_dim)+
      theme_void()
    
    girafe(ggobj = g,
          options = list(
     opts_hover(css = "fill:white !important ;fill-opacity: 1; stroke:blue !important; ")
    ) 
    )
  } else {
    ggplot()+
      geom_stars(data = access)+
      geom_sf(data = commTypes_byMuni, size=.2, 
              fill = 'white', alpha= .001,
              color = 'light gray')+
      geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",#trans= 'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
      theme_void()+
      theme(text=element_text(family="Helvetica"))
  }
}

jobs_access_vis <- visualize_for_access2(jobs %>% select(contains("30min")))
jobs_access_vis
highered_access_vis <- visualize_for_access(highered)
highered_access_vis
test <- highered*dasy_raster$pop_dec_adult
plot(test$HigherEd_MD_TransitAll_30min)


# Find weighted average region wide and  for sub-regions ####
# The weighted average results in the average number of opportunities available to a person within a given aggregation area (or for the entire MPO).
# To do this, we use the weighted_mean function, where the population within the cell is the weight and the opportunities are the value impacted by the weights.
# The weighted average function then needs to be applied to every layer within a an access object, to all access objects, and to all aggregation areas.

# first, make a function that takes a single layer and the corresponding weights and reports a weighted average for all of the variables
get_weighted_avgs <- function(access_layer, weights){
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
        grepl("minority_adult", pop) ~ "Minority status, adult",
        grepl("min", pop) ~ "Minority status",
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
        pop == "zero_veh_hh" ~ "Zero-vehicle households",
        pop == "non_zero_veh_hh" ~ "Non-zero-vehicle households",
        grepl("pop_dec_adult", pop)~ "Total population, adult",
        grepl("pop_dec", pop) ~ "Total population",
        grepl("hh_dec", pop) ~ "Total households"))
  return(weighted_avgs)
}

# next, we need to apply the get weighted avgs to all layers within a stars access object, 
# then to all of the stars objects, 
# and to all of the different aggregation areas

# set up list of access object to loop to get through all access layers
access_all <- list(healthcareNonEmg, healthcareEmg, jobs, essentialplaces, highered, openspace, openspace_paths, openspace_conservation)
# set up community types filters to call on in for loop
comm_filters <- comm_types_id %>% 
  mutate(comm_filter = map(.x= id, ~select(comm_types_rast, paste0("id", .x))))

# loop through all of the access objects
access_all_avgs <- tibble()
for (i in 1:length(access_all)){
  access <- access_all[i][[1]]
  # get weighted average for the entire mpo
  avgs_mpo <- lapply(access, get_weighted_avgs, weights= dasy_raster)
  avgs_mpo <- plyr::ldply(avgs_mpo, data.frame) %>% 
    mutate(region = "MPO")
  
  # get weighted averages for each aggregation area
  access_avgs_agg <- tibble()
  for(j in 1: 7){
    agg <- j
    if (agg == 1) { comm_filter <- comm_types_rast %>% select(id1)}
    if (agg == 2) { comm_filter <- comm_types_rast %>% select(id2)}
    if (agg == 3) { comm_filter <- comm_types_rast %>% select(id3)}
    if (agg == 4) { comm_filter <- comm_types_rast %>% select(id4)}
    if (agg == 5) { comm_filter <- comm_types_rast %>% select(id5)}
    if (agg == 6) { comm_filter <- comm_types_rast %>% select(id6)}
    if (agg == 7) { comm_filter <- comm_types_rast %>% select(id7)}
    
    if (agg == 1) { access_agg <- access * (comm_types_rast %>% select(id1))}
    if (agg == 2) { access_agg <- access * (comm_types_rast %>% select(id2))}
    if (agg == 3) { access_agg <- access * (comm_types_rast %>% select(id3))}
    if (agg == 4) { access_agg <- access * (comm_types_rast %>% select(id4))}
    if (agg == 5) { access_agg <- access * (comm_types_rast %>% select(id5))}
    if (agg == 6) { access_agg <- access * (comm_types_rast %>% select(id6))}
    if (agg == 7) { access_agg <- access * (comm_types_rast %>% select(id7))}
    
    # apply the filter to the population raster
    # removes areas outside of the aggregation area
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
rm(avgs, dasy_filtered, comm_filter, access, access_agg)

access_all_avgs2 <- access_all_avgs %>%
  rowwise() %>% 
  mutate(time= str_sub(.id, start= -5, end = -4),
         destination = case_when(
           grepl('HealthCare_NonEmergency', .id) ~ "Healthcare, non-emergency",
           grepl('HealthCare_Emergency', .id) ~ "Healthcare, emergency",
           grepl('Jobs', .id) ~"Jobs", 
           grepl("EssentialPlaces", .id) ~ "Essential Places",
           grepl("HigherEd", .id) ~ "Higher Education",
           grepl("OpenSpace_Conservation", .id) ~ "Open Space, large parks",
           grepl("OpenSpacePaths", .id) ~ "Open Space, paths",
           grepl("OpenSpace", .id) ~ "Open Space, all parks",
           TRUE ~ NA_character_),
         mode = case_when(
           grepl("Walk", .id) ~ "Walk",
           grepl("Bike", .id) ~ "Bike",
           grepl("Drive", .id) ~ "Drive",
           grepl("TransitA", .id) ~ "Transit (All modes)",
           grepl("TransitBusRT", .id) ~ "Transit (Bus and RT only)",
           TRUE ~ NA_character_)) %>% 
  select(-name,  -.id, -value)

# calculate ratios to compare EJ to non-EJ
# where there is parity the ratio will be one. 
access_all_ratios <-access_all_avgs2 %>% 
  mutate(type_detail = case_when(
    grepl('Total', type)~ "Total",
    grepl("Non", pop_name) ~ "Non-EJ",
    TRUE ~ "EJ"),
    type2 = ifelse(grepl("adult", pop_name), "Adult", NA)) %>% 
  pivot_wider(id_cols = c(destination, mode, time, region, type, type2), names_from = type_detail, values_from = c(AvgAccessOpps) ) %>% 
  rowwise() %>% 
  mutate(Ratio = round(EJ/`Non-EJ`, 3)) %>% 
  select(destination, mode, time, everything()) %>% 
  select(-type2) %>% 
  mutate_if(is.numeric, round, digits=3)

access_all_ratios_MPO  <- access_all_ratios %>% 
  filter(region == "MPO")

access_all_comp <- access_all_ratios %>% 
  filter(region != "MPO") %>% 
  left_join(select(access_all_ratios_MPO, -c(region, Total, EJ, `Non-EJ`)), 
            by = c("destination", "mode", "time", "type"),
            suffix = c(" Aggregation Area", " MPO")) %>% 
  bind_rows(rename(access_all_ratios_MPO, `Ratio MPO`  = Ratio))



# SAVE RESULTS ####
write_csv(access_all_comp, "output/access_all_comp.csv")
