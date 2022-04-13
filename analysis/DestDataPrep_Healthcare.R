# DESTINATION DATA CLEANING ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.

# PACKAGES ###
library(tidyverse)
library(readxl)
library(sf)
library(mapview)

#FUNCTIONS ####
source("functions/points_cleaning.R")
`%notin%` <- Negate(`%in%`)

boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(26986)

# STEP 1: read in data from the data folder ####
# UPDATE: data folder name
datafolder <- "data/Access to healthcare"
filenames_all <- list.files(datafolder)

# Read in excel files
# Note: name of file is based on location of space in the file name
filenames_xl <- filenames_all[endsWith(filenames_all, "USE.xlsx")]
for (i in 1:length(filenames_xl)){
 name <- str_split(filenames_xl[i], " ")[[1]][1]
 xl_data <- read_excel(paste0(datafolder, "/", filenames_xl[i]))
 assign(name, xl_data)
}

# Read in shapefiles
# unzip shapefile to temp folder and read into R
if( dir.exists("data/temp")){sapply(paste0("data/temp/", list.files("data/temp")), unlink)}
unzip(paste0(datafolder, "/",  filenames_all[endsWith(filenames_all, ".zip")] ), exdir= "data/temp")
filenames_shp <- list.files("data/temp")[grepl(".shp$",list.files("data/temp"))]
shp <- read_sf(paste0("data/temp/", filenames_shp))
hospitals <- shp

rm(shp, xl_data, filenames_xl, name, datafolder,filenames_all, filenames_shp, i)

# STEP 2: Prep addresses for geocoding #### 
# Prep non-spatial data for geocoding
# Tidygeocoder has a methods argument, 
# Tried using Open Street Map's Nominatim, but results were not as clean as method = "arcgis"
# When method = "arcgis" results are analogous to running addresses through ArcGIS geoproccessing
# https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html

# address_cl <- `licensed-clinic-services-february-2022` %>% 
#   select(name= `Clinic Name`, addr_st = Street, town= `City/Town`, zip = `Zip Code`) %>% 
#   mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
#   select(name, address) %>% 
#   mutate(type= "Licensed Clinic")
# write_csv(address_cl, "geocoding/clinics_addresses.csv")
rm(`licensed-clinic-services-february-2022`)
# STEP 3: Geocode ####

# library(tidygeocoder)
# pt_cl <- address_cl %>% 
#   geocode(address, method= 'arcgis', lat = latitude , long = longitude) %>% 
#   st_as_sf(coords= c("longitude", "latitude"), crs=4326 )

# Geocoded using MassGIS Locator file in ArcGIS Pro
pt_cl <- read_sf("geocoding/Clinics_Geocoded.geojson") %>% 
  select(name = USER_name, address = USER_address, type= USER_type) %>% 
  st_transform(4326)

# note community health centers had spatial info stored as a character string,
# using that point instead of geocoding
pt_ch <- CommunityHealthCenters_Oct2019 %>% 
  select(name= site_name, addr_st= address, town= mail_city, zip = zip, shape) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
  select(name, address, shape) %>% 
  rowwise() %>% 
  mutate(x = substring(str_split(shape, " ")[[1]][2],2),
         y = substring(str_split(shape, " ")[[1]][3],1,nchar(str_split(shape, " ")[[1]][3])-1)) %>%
  select(-shape) %>% 
  st_as_sf(coords= c("x", "y"), crs= 26986) %>% 
  st_transform(4326) %>% 
  mutate(type= "Community Health Center")
rm(CommunityHealthCenters_Oct2019)
# STEP 4: Find duplicate healthcare opportunities ####
# for the clinics and community health centers there is some overlap between layers. 
# Compare the layers to make sure that the same clinic or CHC isn’t being counted twice, and if it is, 
# remove the offending record from one point layer. Note - hospitals often have complexes with clinics 
# or CHCs or pharmacies, these do not need to be removed.

dup_ch_cl <- find_potential_duplicate_points2(pt_ch, pt_cl)
dup_ch_cl <- dup_ch_cl %>% 
  # inspect and clean up so that all potential duplicates actually appear to be distinct opportunities
  filter(address.1 %notin%  c("35 CONGRESS STREET SUITE 225, SALEM, MA 01970", 
                              "35 CONGRESS ST SHETLAND PK BLD2 215, SALEM, MA 01970"))
pt_ch <- remove_duplicate_points2(pt_ch, pt_cl, dup_ch_cl)


# STEP 5: Compile into single layer ####
healthcare <- hospitals %>% 
  select(name= NAME, addr_st= ADDRESS, town= TOWN, zip= ZIPCODE ) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
  select(name, address) %>% 
  mutate(type = "Hospital") %>% 
  st_transform(4326) %>% 
  bind_rows(pt_ch, pt_cl) %>% 
  st_transform(26986) %>% 
  st_filter(boundary, .predicate = st_within) %>% 
  mutate(id = row_number())


mapview(healthcare, zcol= "type")

# SAVE DATA ####
st_write(healthcare, "output/DestinationData.gpkg", "healthcare_PT")

# SAVE DATA FOR CONVEYAL ####
# note: Conveyal can't have any other numeric fields beside a weight. 
# if there is an id or an FID (default saving with shp driver) then that id becomes the weight and throws off access opportunity counts
healthcare_conveyal <- st_read("output/DestinationData.gpkg", "healthcare_PT") %>% 
  prep_pt_to_csv_keepID()

healthcare_csv<- healthcare_conveyal %>%   
  pt_to_csv("output/healthcare.csv")

healthcare_emergency <- healthcare_conveyal %>% 
  filter(type == "Hospital") %>% 
  pt_to_csv("output/healthcare_emergency.csv")
