library(tidyverse)
library(sf)
library(stars)

# Model result outputs summarized here:
# https://www.ctps.org/data/pdf/plans/LRTP/destination/Destination-2040-LRTP-20191030.pdf#page=243

# SETUP INPUTS ####
boundary<-st_read("output/AggregationAreas.gpkg", layer= "MPO_Boundary") %>% 
  st_transform(3857) # needs to be psudo-mercator for raster operations
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

write_rds(comm_types_id, "app/data/comm_types_id.rds")
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

jobs_access_vis <- visualize_for_access(jobs_access)
highered_access_vis <- visualize_for_access(highered_access)
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

# test function for one layer
highered_TransitAll_30min <- get_weighted_avgs_mpo(highered_access$HigherEd_MD_TransitAll_30min, dasy_raster) %>% 
  mutate(Region = "MPO")
# setup apply statement for all layers
test<- lapply(highered_access, get_weighted_avgs_mpo)
test2 <- plyr::ldply(test, data.frame) %>% 
  mutate(region = "MPO")

# set up loop to get through all access layers
access_all <- list(healthcareNonEmg, healthcareEmg, jobs, essentialplaces, highered, openspace, openspace_paths, openspace_conservation)
access_all_avgs <- tibble()
for (i in 1:length(access_all)){
  # i <- 2
  access <- access_all[i][[1]]
  avgs <- lapply(access, get_weighted_avgs_mpo)
  avgs <- plyr::ldply(avgs, data.frame) %>% 
    mutate(region = "MPO")
  access_all_avgs <- bind_rows(access_all_avgs,
                               avgs)
}


get_weighted_avgs <- function(access_layer, weights){
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
comm_filters <- comm_types_id %>% 
  mutate(comm_filter = map(.x= id, ~select(comm_types_rast, paste0("id", .x))))
test <- tibble(
  dest_type = c("Healthcare, Non-emergency", "Healthcare, Emergency", 
                "Jobs", "Essential Places", "Higher Education", 
                "Open Space", "Open Space, Paths", "Open Space, Conservation")) %>% 
  mutate(access= access_all) %>% 
  full_join(comm_filters, by = character()) %>% 
  mutate(avgs= map2(.x= access, .y= comm_filter, ~.))
  


# weighted avg be subregion
comm_filter <- comm_types_rast %>% select(id1)
#plot(comm_filter)
access_filtered <- openspace * comm_filter
#access_filtered<- access_filtered %>% select(-contains("Drive"))
access_filtered_plot <- visualize_for_access(access_filtered)
access_filtered_plot
dasy_filtered <- dasy_raster * comm_filter
avgs <- get_weighted_avgs(access_filtered$OpenSpace_Weekend_Bike_30min, dasy_filtered) %>%
  enframe() %>% 
  rename(Population= name, AvgAccessOpps = value) %>% 
  mutate(Region = "1")


# 2/3 access to jobs by transit.
# for every two jobs accessible by a minority adult there are three jobs accessible by a nonminority adult
# with in the region there is 9 minority people for every 20 nonminority people



# Population Weighting ####



jobs_minWeighted <- jobs*select(dasy_raster, minority_adult)
names(jobs_minWeighted) <- paste0(names(jobs_minWeighted), "_min")
jobs_nonminWeighted <- jobs*select(dasy_raster, nonminority_adult)
names(jobs_nonminWeighted) <- paste0(names(jobs_nonminWeighted), "_nonmin")
jobs_popWeighted <- jobs*select(dasy_raster, pop_dec_adult)
names(jobs_popWeighted) <- paste0(names(jobs_popWeighted), "_pop")

jobs_weighted <- c(jobs_minWeighted,
          jobs_nonminWeighted,
          jobs_popWeighted)


weights <- dasy_raster %>% select(pop_dec_adult, minority_adult, nonminority_adult, lowinc_adult, nonlowinc_adult)
jw <- c(jobs, weights)
jobs_sf <- jobs %>% st_as_sf()





st_write(jobs_sf, "GIS/access_poly.gpkg", layer= 'jobs3')

test <- jobs %>%select(ends_with("_pop")) %>%  st_redimension()
names(test)<- 'value'
ggplot()+
  geom_stars(data = test)+
  geom_sf(data=boundary,size=.4,color="light gray", fill= NA)+
  coord_sf()+
  scale_fill_gradient(low= 'white', high= '#871F78' ,trans= 'log1p',na.value = "transparent")+
  # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
  facet_wrap(~new_dim)+
  theme_void()




ggplot()+
  geom_stars(data = test%>% select(Jobs_AM_Drive_30min_pop))+
  geom_sf(data=boundary,size=.3,color="light gray", fill= NA)+
  coord_sf()+
  
  # scale_fill_steps2(
  #   space = "Lab",
  #   na.value = "transparent",
  #   guide = "coloursteps",
  #   aesthetics = "fill",
  #   trans = 'log'
  # )+
  # scale_fill_stepsn(n.breaks = 9, colours = viridis::magma(9))+
  # https://colordesigner.io/gradient-generator
  # scale_fill_gradientn(
  #    colors = c('#fffffc','#fffbcc','#fff09c','#ffdd6d',
  #                                  '#ffc43d','#ffa30d','#dc7700','#ad5000','#7d3000','#4d1800'),
  #   #colours = terrain.colors(10),
  #   #trans = 'log1p',
  #   na.value = NA,)+
  # scale_fill_viridis_c(option = "D", na.value = "transparent")+
   #                    name = paste0(destination, "\n Opportunities Accessible"))+
  # scale_color_viridis_c(option = "D", na.value = "transparent",
  #                      name = paste0(destination, "\n Opportunities Accessible"))+
  # ggtitle(paste0("Access to ", destination, "\nwith ",
  #                modes, "\nin ", travel_time))+
  # labs(caption= paste0("Time period: ", time_period))+
  theme_void()


#https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
#https://bluegreenlabs.org/post/map-building-3/

# Combo conveyal runs by destination ####

demo <-dasy_demo %>% 
  select( pop_dec_adult, hh_dec, minority_adult, nonminority_adult,lowinc_adult, nonlowinc_adult, zero_veh_hh, non_zero_veh_hh)
access <- jobs %>% st_as_sf() %>%
  rename(bi_15 = Jobs_AM_Bike_15min_50pct.geotiff,
         bi_30 = Jobs_AM_Bike_30min_50pct.geotiff,
         dr_30 = Jobs_AM_Drive_30min_50pct.geotiff,
         ta_30 = Jobs_AM_TransitAll_30min_50pct.geotiff,
         tb_30 = Jobs_AM_TransitBusRT_30min_50pct.geotiff,
         wa_15 = Jobs_AM_Walk_15min_50pct.geotiff,
         wa_30 = Jobs_AM_Walk_30min_50pct.geotiff
         )
# st_write(access, "GIS/access_poly.gpkg", layer= 'jobs')
demo_access<- demo %>% 
  st_join(access, largest = T)

regional_weighted_avg <- demo_access %>% 
  st_drop_geometry() %>% 
  summarize(
    # pct_min_adult = sum(minority_adult)/(sum(minority_adult)+sum(nonminority_adult)),
    # pct_lowinc_adult = sum(lowinc_adult)/(sum(lowinc_adult)+sum(nonlowinc_adult)),
    min_adult_ta = weighted.mean(ta_30, minority_adult),
            min_adult_tb = weighted.mean(tb_30, minority_adult),
            nonmin_adult_ta = weighted.mean(ta_30, nonminority_adult),
            nonmin_adult_tb = weighted.mean(tb_30, nonminority_adult),
            lowinc_adult_ta = weighted.mean(ta_30, lowinc_adult),
            lowinc_adult_tb = weighted.mean(tb_30, nonlowinc_adult),
            nonlowinc_adult_ta = weighted.mean(ta_30, nonlowinc_adult),
            nonlowinc_adult_tb = weighted.mean(tb_30, nonlowinc_adult),
            min_adult_dr = weighted.mean(dr_30, minority_adult),
            nonmin_adult_dr = weighted.mean(dr_30, nonminority_adult),
            lowinc_adult_dr = weighted.mean(dr_30, lowinc_adult),
            nonlowinc_adult_dr = weighted.mean(dr_30, nonlowinc_adult),
            min_adult_wa = weighted.mean(wa_30, minority_adult),
            nonmin_adult_wa = weighted.mean(wa_30, nonminority_adult),
            lowinc_adult_wa = weighted.mean(wa_30, lowinc_adult),
            nonlowinc_adult_wa = weighted.mean(wa_30, nonlowinc_adult),
            min_adult_bi = weighted.mean(bi_30, minority_adult),
            nonmin_adult_bi = weighted.mean(bi_30, nonminority_adult),
            lowinc_adult_bi = weighted.mean(bi_30, lowinc_adult),
            nonlowinc_adult_bi = weighted.mean(bi_30, nonlowinc_adult)) %>% 
  t()



### BY AGGREGATION AREA ####
access_agg_area <- demo_access %>% 
  mutate(area_grid = st_area(geom)) %>% 
  st_intersection(select(comm_types, municipality, communityType, subtype)) %>% 
  mutate(area = st_area(geom),
         area_frac = units::drop_units(area/area_grid)) %>% 
  select(-c(area, area_grid))

# comm_type_id <- comm_types %>% 
#   select(municipality, communityType, subtype) %>% 
#   st_drop_geometry() %>% 
#   group_by(communityType) %>% 
#   mutate(comm_id = row_number()) %>% 
#   ungroup() %>% 
#   group_by(communityType, subtype) %>% 
#   mutate(subtype_id = row_number()) 

test <- select(comm_types, municipality, communityType, subtype) %>% 
  st_rasterize(template= prep_grid)

test_comb <- c(test, jobs)

mapview(test) + mapview(access)

agg_summary <- access_agg_area %>% 
  group_by(communityType, subtype) %>% 
  summarize(
    tot_min_adult = sum(minority_adult),
    tot_nonminadult = sum(nonminority_adult),
    tot_lowinc_adult = sum(lowinc_adult),
    tot_nonlowinc_adult = sum(nonlowinc_adult),
    # min_adult_opps
    # min_adult_by_transitall = weighted.mean(transitall_30, minority_adult),
    # min_adult_by_transitbusrt = weighted.mean(trasnitbusrt_30, minority_adult),
    # nonmin_adult_by_transitall = weighted.mean(transitall_30, nonminority_adult),
    # nonmin_adult_by_transitbusrt = weighted.mean(trasnitbusrt_30, nonminority_adult),
    # lowinc_adult_by_transitall = weighted.mean(transitall_30, lowinc_adult),
    # lowinc_adult_by_transitbusrt = weighted.mean(trasnitbusrt_30, nonlowinc_adult),
    # nonlowinc_adult_by_transitall = weighted.mean(transitall_30, nonlowinc_adult),
    # nonlowinc_adult_by_transitbusrt = weighted.mean(trasnitbusrt_30, nonlowinc_adult),
    # min_adult_by_waive = weighted.mean(waive_30, minority_adult),
    # nonmin_adult_by_waive = weighted.mean(waive_30, nonminority_adult),
    # lowinc_adult_by_waive = weighted.mean(waive_30, lowinc_adult),
    # nonlowinc_adult_by_waive = weighted.mean(waive_30, nonlowinc_adult),
    geom = st_union(geom))



j = st_redimension(jobs)

destination <- "Employment"
ggplot()+
  geom_stars(data=j)+
  # geom_sf(data=access_contours, aes(color = value),size=.8, show.legend = F)+
  # geom_contour(data=t)+
  geom_sf(data=boundary,size=1,color="light gray", fill= NA)+
  coord_sf()+
  facet_wrap(~new_dim)+
  scale_fill_viridis_c(option = "D", na.value = "transparent",
                        name = paste0(destination, "\n Opportunities Accessible"))+
  # # scale_color_viridis_c(option = "D", na.value = "transparent",
  # #                      name = paste0(destination, "\n Opportunities Accessible"))+
  # ggtitle(paste0("Access to ", destination, "\nwith ",
  #                modes, "\nin ", travel_time))+
  # labs(caption= paste0("Time period: ", time_period))+
  theme_void()
  
