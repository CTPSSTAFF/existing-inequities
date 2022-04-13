# DESTINATION DATA CLEANING ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.

# PACKAGES ###
library(tidyverse)
library(readxl)
library(sf)
library(mapview)
`%notin%` <- Negate(`%in%`)

boundary<- read_rds("data/boundary.rds") %>% 
  st_transform(26986)


# STEP 1: read in data from the data folder ####
# UPDATE: data folder name
datafolder <- "data/Access to higher ed"
filenames_all <- list.files(datafolder)


filenames_xl <- filenames_all[endsWith(filenames_all, ".xlsx")]
enrollment <- read_excel(file.path(datafolder, filenames_xl[1]))

if( dir.exists("data/temp")){sapply(paste0("data/temp/", list.files("data/temp")), unlink)}
unzip(paste0(datafolder, "/",  filenames_all[endsWith(filenames_all, ".zip")] ), exdir= "data/temp")
filenames_shp <- list.files("data/temp")[grepl(".shp$",list.files("data/temp"))]
shp <- read_sf(paste0("data/temp/", filenames_shp))
colleges <- shp
rm(shp)

# filter colleges shapefile so only mpo colleges
colleges <- colleges %>% 
  st_filter(boundary, .predicate = st_within)




# Prep to join enrollment to colleges ####
e_prep <- enrollment %>% 
  mutate(id = substr(NCES_ID,1,6)) %>% 
  select(id,e_NCES_ID= NCES_ID, e_name = College, tot_enroll_19= Total_Enrollment_2019AY...10, e_address = Address, e_city = City) %>% 
  mutate(e_name = case_when(
     e_name == "Boston University School of Medicine" ~ 	
      "Boston University School of Medicine (Medical Campus)",
     TRUE ~ e_name))
c_prep <- colleges %>% 
  mutate(id = substr(NCES_ID, 1, 6)) %>% 
  mutate(c_name = ifelse(is.na(CAMPUS)==F, paste0(COLLEGE, " (", CAMPUS, ")"), COLLEGE)) %>% 
  select(c_name, COLLEGE, CAMPUS, id, c_NCES_ID= NCES_ID) 

ec <- e_prep %>% 
  # left_join(c_prep, by= c("id"= "id")) %>%
#    mutate(flag =ifelse(e_name != c_name, T, F))
    left_join(c_prep, by= c("id"= "id", "e_name"= "c_name")) %>% 
# 
# test <- e_prep %>% 
#   anti_join(c_prep, by= c("id"= "id", "e_name"= "c_name"))


  # group_by(id) %>% 
  arrange(id) #%>%

ec_issue <- ec %>% 
  filter(is.na(c_NCES_ID)==T)

ec_check <- ec %>% 
  filter(is.na(c_NCES_ID)==F) %>% 
  st_as_sf()
mapview(ec_check, cex = "tot_enroll_19")+ boundary
