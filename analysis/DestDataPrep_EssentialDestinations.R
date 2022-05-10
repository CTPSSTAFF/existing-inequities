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
datafolder <- "data/Access to Essential Places"
filenames_all <- list.files(datafolder)

# note: do have to read in all xl files, instead pull in healthcare and post offices are already geocoded.
# pull in prepared healthcare locations, see "DestDataPrep_Healthcare.R"
healthcare <- st_read("output/DestinationData.gpkg", "healthcare_PT") %>% 
  rename(geometry= geom)

# Step 1A: Set up pharmacies for geocoding ###
# Retail Pharmacies ####
# retail_pharmacies<- readxl::read_excel(file.path(datafolder, "Retail_Pharmacies.xlsx"))
# address_pharma <- retail_pharmacies %>% 
#   mutate(address = paste0(addr_line_2, ", ", addr_line_4)) %>% 
#   select(name = full_name, address) %>% 
#   mutate(type= "Retail Pharmacy")
# write_csv(address_pharma, "geocoding/pharma_addresses.csv")
# Geocoded in ArcGIS Pro using MassGIS Locator file. Did not mannually sort through 
# matches for pharmacies in municiaplities outside the mpo region
retail_pharmacies<- read_sf("geocoding/Pharma_Geocoded.geojson") %>% 
  select(name = USER_name, address = USER_address, type= USER_type) %>% 
  st_transform(26986) %>%
  st_filter(boundary, .predicate = st_within)

# Read in shapefiles
# unzip shapefile to temp folder and read into R
filenames_zips <- filenames_all[endsWith(filenames_all, ".zip")]
for (i in 1: length(filenames_zips)){
  # clear out temp folder
  if(i == 1 & dir.exists("data/temp")){sapply(paste0("data/temp/", list.files("data/temp")), unlink)}
  # pull file name from zip
  name <- str_split(filenames_zips[i], ".zip")[[1]][1]
  # unzip files to temp folder
  unzip(paste0(datafolder, "/",  filenames_zips[i] ), exdir= "data/temp")
  
  filenames_shp <- list.files("data/temp")[grepl(".shp$",list.files("data/temp"))]
  for (j in 1: length(filenames_shp)){
  shp <- read_sf(paste0("data/temp/", filenames_shp[j]))
  shp_name <- str_split(filenames_shp[j], ".shp")[[1]][1]
  assign(shp_name, shp)
  }
  rm(shp)
}


rm(i, j, datafolder, filenames_all, filenames_shp, filenames_zips, name, shp_name)
# STEP 2: Clean up data ####

townhalls <- TOWNHALLS_PT_MEMA %>% 
  mutate(type = "Townhall",
         address = paste0(ADDRESS, ", ", CITY, ", MA ", "ZIP")) %>% 
  select(name= NAME, type, address) %>% 
  st_transform(26986) %>% 
  st_filter(boundary, .predicate = st_within)
rm(TOWNHALLS_PT_MEMA)

postoffices <- BRMPO_PostOffices %>% 
  select(name = PO_Names, address = Match_addr) %>% 
  mutate(type = "Post Office") %>% 
  st_transform(26986) %>% 
  st_filter(boundary, .predicate = st_within)
rm(BRMPO_PostOffices)

libraries <- LIBRARIES_PT %>% 
  # do not include libraries where type is "SPECIAL"
  filter(TYPE %notin% c("SPECIAL")) %>% 
  select(name= NAME, addr_st= ADDRESS, town= TOWN, zip = ZIP) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip ),
         type= "Library") %>% 
  select(name, address, type)%>% 
  st_transform(26986) %>% 
  st_filter(boundary, .predicate = st_within)
rm(LIBRARIES_PT)

# Merged winter and summer duplicate points
# Found duplicate points based on geography
# Pulled first name and address based on order in the starting dataset.
farmersmarkets <- FARMERSMARKETS_PT %>% 
  mutate(address= paste0(ADDR_1, ", ", TOWN, ", MA ", ZIP_CODE)) %>% 
  select(name= NAME, TYPE, address)%>% 
  st_filter(boundary, .predicate = st_within)
fm_unique_geog <- farmersmarkets%>% 
  select(geometry) %>% 
  distinct() %>% 
  mutate(id = row_number())
farmersmarkets<- fm_unique_geog %>% 
  st_join(farmersmarkets) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  summarize(name = first(name),
            address = first(address)) %>% 
  left_join(fm_unique_geog, by = "id") %>% 
  st_as_sf() %>% 
  st_transform(26986) %>% 
  mutate(type= "Farmers' Markets")
rm(FARMERSMARKETS_PT)

# include where primary type “Meat Markets,” “Fish and Seafood Markets,” “All Other Specialty Food Stores,” 
# “Supermarkets/Other Grocery,” “Fruit & Vegetable Markets,” “Warehouse Clubs & Supercenters,” and “Department Stores.” 
grocery <- `export-gisdata.mapc.food_retailers_2017_pt` %>% 
  filter(prim_type %in% c("Meat Markets", "Fish & Seafood Markets", "All Other Specialty Food Stores",
                          "Supermarkets/Other Grocery (Exc Convenience) Strs","Fruit & Vegetable Markets",
                          "Warehouse Clubs & Supercenters","Department Stores (Except Discount Dept Stores)" 
                          )) %>% 
  mutate(type = "Grocery",
         address = paste0(address, ", ", municipal, ", MA ", zipcode)) %>% 
  select(name, type,type_detail = prim_type, address)%>% 
  st_filter(boundary, .predicate = st_within)

rm(`export-gisdata.mapc.food_retailers_2017_pt`)

# STEP 3: Bind Together essential destinations ####
# Bring essential places together
essential_destinations <- healthcare %>% 
  bind_rows(townhalls, postoffices, libraries, grocery, retail_pharmacies, farmersmarkets) %>% 
  select(-id) %>% 
  mutate(id= row_number())
mapview(essential_destinations, zcol = "type")


# SAVE DATA ####
st_write(essential_destinations, "output/DestinationData.gpkg", "essential_destinations_PT", append = T)


# SAVE DATA FOR CONVEYAL ####
# We don't need to save data for Conveyal because we will save the central destination within a cluster

