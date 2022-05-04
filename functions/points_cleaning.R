# Spatial functions to clean destination points and prep for Conveyal upload

library(tidyverse)
library(sf)


# Function reports if there are near duplicate points across two point files. 
# Inputs are two sf point layers and a buffer distance
# Output is a subset of points1 that are potentially duplicated in points2
find_potential_duplicate_points2 <- function(points1, points2, buffer= 10){
  
  # identical(st_crs(points1), st_crs(points2))
  proj_start <- st_crs(points1)
  
  # reproject points to ensure using meters as unit of length
  points1 <- points1 %>% st_transform(26986)
  points2 <- points2 %>% st_transform(26986)
  
  
  # buffer points2
  pt2_buff <- points2 %>% 
    st_buffer(buffer)
  
  # set attribute values to constant to avoid warning message
  # https://stat.ethz.ch/pipermail/r-sig-geo/2017-July/025814.html
  st_agr(points1) <- "constant"
  st_agr(pt2_buff) <- "constant"
  
  potential_dups <- points1 %>% 
    st_intersection(pt2_buff) %>% 
    st_transform(proj_start)
  
  return(potential_dups)
  
}

remove_duplicate_points2 <- function(points1, points2, dups){
  # points1<- pt_ch
  # dups <- dup_ch_cl
  # remove duplicate points from points1 so that they only show up in points2
  
  dups <- dups %>% st_drop_geometry()
  points1_no_dup <- points1 %>%
    left_join(dups) %>% 
    filter(is.na(name.1)==T) %>% 
    select(name, address,type, geometry) %>% 
    st_as_sf()
return(points1_no_dup)  
}



# Function reports if there are near duplicate points within the same sf point file. 
# Inputs are a sf point layers and a buffer distance
# Output is a subset of points1 that are potentially duplicated within other entries
# find_duplicate_points1 <- function(points1, buffer){
#   
#   
#   return(potential_dups)
# }
# 
# remove_duplicate_points1 <- function(points1, dups){
#   
# }

# from sf goem to point csv for conveyal upload
# note: the only numberic field in the layer should be the weight field
prep_pt_to_csv <- function(pt){
  pt <- pt %>% 
    rename(geometry= geom) %>%
    select(geometry, type) %>% 
    st_transform(4269)
  
  return(pt)
}

prep_pt_to_csv_keepID <- function(pt){
  pt <- pt %>% 
    rename(geometry= geom) %>%
    select(geometry, type, id) %>% 
    st_transform(4269)
  
  return(pt)
}

prep_pt_to_csv_keepID_weight <- function(pt){
  pt <- pt %>% 
    rename(geometry= geom) %>%
    select(geometry, type, id, weight) %>% 
    st_transform(4269)
  
  return(pt)
}

pt_to_csv <- function(pt, output){
  pt<- pt %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat =st_coordinates(.)[,2]) %>% 
    st_drop_geometry() %>% 
    write_csv(output)
  return(pt)
}
