# DESTINATION DATA CLEANING ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.

# PACKAGES ###
library(tidyverse)
library(sf)
library(mapview)
`%notin%` <- Negate(`%in%`)
source('functions/points_cleaning.R')
boundary<- read_rds("data/boundary.rds") 
# brookline <- ma_muni_geog %>% # from agg boundaries muni pull
#   filter(grepl("Brookline", NAME)) %>% 
#   st_transform(26986)


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

# Open space
# Select those that are at least partially in the MPO region (no minimum percentage.).
# Select on the following fields: PRIM_PURP = C [conservation], R [recreation], or B [recreation and conservation], and field PUB_ACCESS = Y [yes]
open_space <- open_space %>% 
  filter(PUB_ACCESS == "Y") %>% 
  filter(PRIM_PURP %in% c("C", "R", "B")) %>% 
  select(name= SITE_NAME, manager = MANAGER ) %>% 
  mutate(type = "Open Space") %>% 
  mutate(area_acres = round(units::drop_units(st_area(.))/4046.8564224,3)) %>% 
  st_filter(boundary, .predicate = st_intersects)

# merge areas where boundaries touch as single open space
open_space_merge <- open_space %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_as_sf()

# Shared Use Paths
# Select those that are at least partially in the MPO region (no minimum percentage.).
paths <- paths %>% 
  select(objectid, local_name) %>% 
  st_filter(boundary, .predicate = st_intersects) 

library(mapview)
open_space_b <- open_space %>% 
  st_filter(brookline, .predicate = st_intersects)

paths_b <- paths %>% 
  st_filter(brookline, .predicate = st_intersects)

mapview(open_space_b)
# merge areas where boundaries touch as single open space
open_space_b_merge <- open_space_b %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_as_sf()

centroid <- open_space_b_merge %>% st_centroid()

open_space_b_merge_outline<-st_cast(open_space_b_merge, "MULTILINESTRING", group_or_split= FALSE)
open_space_b_merge_outline_pt <- open_space_b_merge_outline %>% 
  st_cast("LINESTRING") %>% 
  st_line_sample(density=1/100) %>% 
  st_as_sf()

mapview(open_space_b_merge_outline)+mapview(open_space_b_merge_outline_pt)

open_space_b_merge_multiline <- st_cast(open_space_b_merge_buf, "MULTILINESTRING", group_or_split= FALSE)
dist_to_edge <- st_nearest_feature(open_space_b_merge_multiline, osm_edges)
open_space_b_merge_buf<- st_buffer()
open_space_b_merge_toNetwork <- st_intersection(osm_edges, 
                                                st_cast(open_space_b_merge_buf, "MULTILINESTRING", group_or_split= FALSE))

mapview(open_space_b_merge)+ mapview(osm_edges)+ mapview(open_space_b_merge_toNetwork)


mapview(test) + centroid
mapview(grid)+ grid_int

paths_as_pt <- paths %>% 
  st_line_sample(1/250)

# TODO: consider buffering paths and combining with open space?
paths_buf <- paths %>% 
  st_buffer(3) # 3 meter buffer to paths

open_wPaths <- open_space %>% 
  # st_buffer(20) %>% # 20m buffer on open space to grab closest network
  st_union() %>%
  st_union(paths_buf) # %>% 
  # st_combine() %>% 
  # st_as_sf()
mapview(open_wPaths)

osm_edges <- st_read("data/BrooklineOSMNetwork.gpkg", layer ="edges") %>% 
  st_transform(26986)

test<- st_intersection(osm_edges, 
                       st_cast(open_wPaths, "MULTILINESTRING", group_or_split= FALSE))

mapview(open_wPaths)+osm_edges+ test +boundary


# SAVE DATA
st_write(open_space, "output/DestinationData.gpkg", "OpenSpace_POLY", append = T)
st_write(paths, "output/DestinationData.gpkg", "Paths_LINE", append = T)
