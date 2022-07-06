# DESTINATION DATA CLEANING ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.

# PACKAGES ####
library(tidyverse)
library(sf)
library(mapview)
# FUNCTIONS ####
`%notin%` <- Negate(`%in%`)
source('functions/points_cleaning.R')

# IPUTS ####
boundary<- read_rds("data/boundary.rds") 


# STEP 1: read in data from the data folder ####
# UPDATE: data folder name
datafolder <- "data/Access to parks and recreation"
filenames_all <- list.files(datafolder)

# Read in shapefiles
if( dir.exists("data/temp")){sapply(paste0("data/temp/", list.files("data/temp")), unlink)}
unzip(paste0(datafolder, "/",  filenames_all[1] ), exdir= "data/temp")
filenames_shp <- list.files("data/temp")[grepl(".shp$",list.files("data/temp"))]
paths <- read_sf(paste0("data/temp/", filenames_shp[1]))

# from OpenSpace GDB, pulled all areas completely within or within 1000km of MPO Boundary
open_space <- read_sf("data/Access to parks and recreation/OpenSpace.shp")
 
# STEP 2: Filter to relevant open spaces ####
# Open space
# Select those that are at least partially in the MPO region (no minimum percentage.).
# Select on the following fields: PRIM_PURP = C [conservation], R [recreation], or B [recreation and conservation], and field PUB_ACCESS = Y [yes]
open_space <- open_space %>% 
  filter(PUB_ACCESS == "Y") %>% 
  filter(PRIM_PURP %in% c("C", "R", "B")) %>% 
  select(name= SITE_NAME, manager = MANAGER, primary_purpose= PRIM_PURP ) %>% 
  mutate(type = "Open Space") %>% 
  mutate(area_acres = round(units::drop_units(st_area(.))/4046.8564224,3)) %>% 
  st_filter(boundary, .predicate = st_intersects)

# merge areas where boundaries touch as single open space
open_space_merge <- open_space %>% 
  group_by(primary_purpose) %>%
  summarize(geometry = st_cast(st_union(geometry), "POLYGON")) %>% 
  st_as_sf() %>% 
  mutate(area = unclass(st_area(geometry))) %>% 
  # filter parks that are less than half an acre in area
  filter(area > 4046.86/2)

open_space_large <- open_space_merge %>% 
  #filter(area > 300000) # approx 75 acres
  filter(area > 500000) # approx 124 acres
#mapview(open_space_large, zcol = ("primary_purpose"))

# Shared Use Paths
# Select those that are at least partially in the MPO region (no minimum percentage.).
paths <- paths %>% 
  select(objectid, local_name) %>% 
  st_filter(boundary, .predicate = st_intersects) 

# Parks to points: Intersection with network
# First intersect with walk network
# For parks that don't intersect with the walk network intersect with drive network
# For parks that don't intersect with either dirve or walk network, buffer by 5m and intersect with drive network

osm_edges_drive <- st_read("notebooks/data/mpo_drive_network.gpkg", layer ="edges") %>% 
  st_transform(26986)
osm_primary_roads <- st_read("notebooks/data/MA_drive_primary.gpkg", layer = "edges") %>% 
  st_transform(26986)
# Remove primary roads from road network
# test <- osm_edges_drive %>% filter(osmid %in% osm_primary_roads$osmid)
drive_network <- osm_edges_drive %>% 
  filter(osmid %notin% osm_primary_roads$osmid)
rm(osm_edges_drive, osm_primary_roads)

walk_network <- st_read("notebooks/data/mpo_walk_network.gpkg", layer ="edges") %>% 
  st_transform(26986)

set_up_network_ints <- function(open_space){
  open_space<- open_space %>%
    mutate(id = row_number())
  return(open_space)
}
find_access_points<- function(open_space, network){
  # network <- network_drive
  # open_space <- open_space_remaining
  
  if(nrow( open_space[network,]) == 0) {
    print("No overlap.")
    empty <-  open_space[network,]
    empty <- empty %>% 
      rename(geom= geometry) %>% 
      mutate(osmid = NA_character_) %>% select(osmid, geom)
    return(empty)
} else {
  int <- st_intersection(select(network,osmid), 
                              st_cast(
                                st_union(open_space), 
                                "MULTILINESTRING", group_or_split= FALSE)) %>%
    group_by(geomtype = st_geometry_type(geom)) %>%
    group_modify(~ st_cast(.x, "POINT")) %>% 
    ungroup() %>% 
    st_as_sf() %>% 
    select(-geomtype)
  
  return(int)
  }
  
}
find_nonintersecting_open_space <- function(open_space, network){
    int <- open_space[network,]
    open_space_remaining<- open_space %>% 
      filter(id %notin% int$id)
  return(open_space_remaining)
}
find_network_ints <- function(open_space, network_walk, network_drive, buffer_dist){
  # open_space <- open_space_large %>% set_up_network_ints()
  # network_walk<-walk_network_muni
  # network_drive <- drive_network_muni
  # buffer_dist <- 3 
  int_walk <- open_space %>% 
    find_access_points(network = network_walk)
  
  open_space_remaining <- open_space %>% 
    find_nonintersecting_open_space(network = network_walk)
  
  int_drive <- open_space_remaining %>% 
    find_access_points(network = network_drive)
  
  open_space_remaining <- open_space_remaining %>% 
    find_nonintersecting_open_space(network =  network_drive)
  
  int_drive_buf <- open_space_remaining %>% 
    st_buffer(buffer_dist) %>% 
    find_access_points(network = network_drive)
  
  network_ints <- int_walk %>% 
    bind_rows(int_drive, int_drive_buf)
  
  return(network_ints)
}

# drive_network_muni <- drive_network %>% st_filter(muni, .predicate = st_intersects)
# walk_network_muni <- walk_network %>% st_filter(muni, .predicate = st_intersects)
network_ints_large <- open_space_large %>% 
  set_up_network_ints() %>% 
  find_network_ints(network_walk = walk_network, 
                    network_drive = drive_network,
                    buffer_dist = 3)
# plot(boundary$geometry, col = 'yellow')
# plot(open_space_large$geometry, col = 'green', border = "black", add = T)
# plot(network_ints_large, col= 'blue', add = T)
# mapview(boundary)+ mapview(open_space_large, col.region= 'green')+ mapview(network_ints_large, cex= 3, color = 'blue')+ paths_pts

network_ints <- open_space_merge %>% 
  set_up_network_ints() %>% 
  find_network_ints(network_walk = walk_network, 
                    network_drive = drive_network,
                    buffer_dist = 3)


# plot(muni$geometry, col = 'yellow')
# plot(open_space_merge$geometry, col = 'green', border = "black", add = T)
# plot(test, col= 'blue', add = T)

paths_pts<- paths%>% 
  st_cast("LINESTRING") %>%
  st_line_sample(density = 1/500) %>% 
  st_as_sf() %>% 
  filter(!st_is_empty(.)) %>% 
  rename(geom= x) %>% 
  group_by(geomtype = st_geometry_type(geom)) %>%
  group_modify(~ st_cast(.x, "POINT")) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  select(-geomtype)

# plot(boundary$geometry)
# plot(paths_pts$geom, add= T)



# SAVE DATA####
st_write(open_space, "output/DestinationData.gpkg", "OpenSpace_POLY", append = T)
st_write(paths, "output/DestinationData.gpkg", "Paths_LINE", append = T)
st_write(paths_pts , "output/DestinationData.gpkg", "Paths_PT", append = T)
st_write(open_space_merge, "output/DestinationData.gpkg", "OpenSpaceAccess_Poly", append= T)
st_write(open_space_large, "output/DestinationData.gpkg", "OpenSpaceAccess_large_POLY", append = T)
st_write(network_ints, "output/DestinationData.gpkg", "OpenSpaceAccess_PT", append = T)
st_write(network_ints_large, "output/DestinationData.gpkg", "OpenSpaceAccess_large_PT", append=T)

# st_write(centroid, "output/OpenSpaceAsPt.gpkg", "centroids_brookline")
# st_write(outline_pt, "output/OpenSpaceAsPt.gpkg", "outline_pt_brookline", append = T)
# st_write(network_ints, "output/OpenSpaceAsPt.gpkg", "network_int_brookline", append = T)

# SAVE DATA for Conveyal test runs
# centroid<- st_read("output/OpenSpaceAsPt.gpkg", "centroids_brookline")
# outline_pt<- st_read( "output/OpenSpaceAsPt.gpkg", "outline_pt_brookline")
# network_ints <- st_read( "output/OpenSpaceAsPt.gpkg", "network_int_brookline")

open_space_large_csv <- network_ints_large %>% 
  mutate(id = row_number(), type= "Open space") %>% 
  select(id, type) %>% 
  prep_pt_to_csv_keepID() %>%   
  pt_to_csv("output/open_space_large_network_access.csv")
open_space_csv <- network_ints %>% 
  mutate(id = row_number(), type= "Open space") %>% 
  select(id, type) %>% 
  prep_pt_to_csv_keepID() %>%   
  pt_to_csv("output/open_space_network_access.csv")
paths_csv <- paths_pts %>% 
  mutate(id = row_number(), type= "Path") %>% 
  select(id, type) %>% 
  prep_pt_to_csv_keepID() %>%   
  pt_to_csv("output/open_space_path_access.csv")

# # Back up park to point options
# # OPTION 1: Cetroids of parks
# centroid <- open_space_b_merge %>% st_centroid()
# centroid_paths <- paths_b %>% st_buffer(10) %>% st_centroid()
# centroid <- bind_rows(centroid, centroid_paths) %>% st_as_sf()
# 
# mapview(open_space_b,color = 'blue', col.region= "blue") + mapview(paths_b, color= "blue")+
#   mapview(centroid, cex= 3, color= "red")
# 
# # OPTION 2: Points along outline
# outline <-st_cast(open_space_b_merge, "MULTILINESTRING", group_or_split= FALSE) 
# 
# outline_pt <- outline %>% 
#   st_cast("LINESTRING") %>% 
#   st_line_sample(density=1/250) %>% 
#   st_as_sf() %>% 
#   filter(!st_is_empty(.)) %>% 
#   group_by(geomtype = st_geometry_type(x)) %>%
#   group_modify(~ st_cast(.x, "POINT")) %>% 
#   ungroup() %>% 
#   st_as_sf() %>% 
#   select(-geomtype)
# 
# pt_path <- paths_b %>% 
#   st_cast("LINESTRING") %>%
#   st_line_sample(density = 1/250) %>% 
#   st_as_sf() %>% 
#   filter(!st_is_empty(.)) %>% 
#   group_by(geomtype = st_geometry_type(x)) %>%
#   group_modify(~ st_cast(.x, "POINT")) %>% 
#   ungroup() %>% 
#   st_as_sf() %>% 
#   select(-geomtype)
# 
# outline_pt <- outline_pt %>%
#   bind_rows(pt_path) %>% 
#   rename(geometry= x) %>% 
#   st_as_sf()
# 
# 
# mapview(open_space_b, color = 'blue', col.region= "blue") + mapview(paths_b, color= "blue")+
#   mapview(centroid, cex= 3, color= "red")+
#   mapview(outline_pt, cex= 3, color= "pink")
