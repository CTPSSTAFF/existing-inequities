library(tidyverse)
library(mapview)
library(sf)

tracts <- st_read("output/DemographicData.gpkg", "tracts_acs_dec_2020") %>% 
  st_transform(4326)

essential_places <- st_read("output/DestinationData.gpkg", layer = "essentialPlace_Final_PT") %>% 
  st_transform(4326)

# origins
# tract centriods
t_centroid <- tracts %>% 
  mutate(geom = st_centroid(geom)) %>%
  st_as_sf() %>% 
  select(name = GEOID) %>% 
  mutate(type = "o")

mapview(t_centroid)

# destinations
ep_top <- essential_places %>% 
  arrange(desc(n)) %>% 
  slice_max(n, n = 50) %>% 
  select(name = cluster, n) %>% 
  mutate(type = "d")

mapview(ep_top)

pts <- bind_rows(t_centroid, ep_top) %>% 
  st_as_sf() %>% 
  mutate(id = row_number())
mapview(pts, zcol = 'type')

st_write(pts, "output/TravelCosts.gpkg", layer = "ODpts")

prep_pt_to_csv_keepID <- function(pt, output){
  if('geom' %in% colnames(pt)){pt <- pt %>% rename(geometry= geom) }
  pt <- pt %>% 
    select(geometry, type, id) %>% 
    st_transform(4326) %>% 
    mutate(lon = st_coordinates(.)[,1],
           lat =st_coordinates(.)[,2]) %>% 
    st_drop_geometry() %>% 
    write_csv(paste0("output/",output, ".csv"))
  return(pt)
}


prep_pt_to_csv_keepID(pts, "ODPoints")


