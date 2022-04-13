# DESTINATION DATA CLEANING ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.

# PACKAGES ###
library(tidyverse)
library(sf)
library(mapview)
`%notin%` <- Negate(`%in%`)

boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(26986)


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
# TODO: Consider merging continuous open space so that a minimum area threshold has more meaning

# Shared Use Paths
# Select those that are at least partially in the MPO region (no minimum percentage.).
paths <- paths %>% 
  st_filter(boundary, .predicate = st_intersects)
# TODO: consider buffering paths and combining with open space?


# SAVE DATA
st_write(open_space, "output/DestinationData.gpkg", "OpenSpace_Area", append = T)
st_write(paths, "output/DestinationData.gpkg", "Paths_Line", append = T)
