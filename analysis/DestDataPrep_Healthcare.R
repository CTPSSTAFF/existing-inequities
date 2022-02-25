# DESTINATION DATA CLEANING ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.

# PACKAGES ###
library(tidyverse)
library(readxl)
library(sf)
library(tidygeocoder)
library(mapview)

# STEP 1: read in data from the data folder ####
# UPDATE: data folder name
datafolder <- "data/Access to healthcare"
filenames_all <- list.files(datafolder)

# Read in excel files
# TODO: check if any "ORIG.xlsx" needs to be read in as well
# Note: name of file is based on location of space in the file name
filenames_xl <- filenames_all[endsWith(filenames_all, "USE.xlsx")]
for (i in 1:length(filenames_xl)){
 name <- str_split(filenames_xl[i], " ")[[1]][1]
 xl_data <- read_excel(paste0(datafolder, "/", filenames_xl[i]))
 assign(name, xl_data)
}

# Read in shapefiles
# unzip shapefile to temp folder and read into R
# TODO: if multiple shapefiles then set up for loop
unzip(paste0(datafolder, "/",  filenames_all[endsWith(filenames_all, ".zip")] ), exdir= "data/temp")
filenames_shp <- list.files("data/temp")[grepl(".shp$",list.files("data/temp"))]
shp <- read_sf(paste0("data/temp/", filenames_shp))
hospitals <- shp

# STEP 2: Prep addresses for geocoding #### 
# Prep non-spatial data for geocoding
# Tidygeocoder has a methods argument, 
# Tried using Open Street Map's Nominatim, but results were not as clean as method = "arcgis"
# When method = "arcgis" results are analogous to runnging addresses through ArcGIS geoproccessing
# https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html

address_cl <- `licensed-clinic-services-february-2022` %>% 
  select(name= `Clinic Name`, addr_st = Street, town= `City/Town`, zip = `Zip Code`) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
  select(name, address) %>% 
  mutate(type= "Licensed Clinic")

# STEP 3: Geocode

pt_cl <- address_cl %>% 
  geocode(address, method= 'arcgis', lat = latitude , long = longitude) %>% 
  st_as_sf(coords= c("longitude", "latitude"), crs=4326 )

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

# STEP 4: Compile into single layer
healthcare <- hospitals %>% 
  select(name= NAME, addr_st= ADDRESS, town= TOWN, zip= ZIPCODE ) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
  select(name, address) %>% 
  mutate(type = "Hospital") %>% 
  st_transform(4326) %>% 
  bind_rows(pt_ch, pt_cl)


mapview(healthcare, zcol= "type")
