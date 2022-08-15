# HIGHER EDUCATION DESTINATION DATA CLEANING ####
## GOAL ####
# Read in files, check for gaps or missing fields, merge into one layer for each destination type.
## DATA SOURCES ####
# colleges and universities:  https://www.mass.gov/info-details/massgis-data-colleges-and-universities

# PACKAGES ###
library(tidyverse)
library(readxl)
library(sf)
library(mapview)
`%notin%` <- Negate(`%in%`)
source("functions/points_cleaning.R")

boundary<- read_rds("data/boundary.rds")


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
     # note Northeastern enrollment not provided by campus, so just using main campus
     e_name == "Northeastern University"~ "Northeastern University (Main Campus)",
     e_name == "University of Massachusetts Boston" ~ "University of Massachusetts Boston (Boston)",
     e_name == "Cambridge College" ~ "Cambridge College (Main Campus)",
     e_name == "Harvard University (Main Campus)" ~ "Harvard College (Main Campus)",
     e_name == "Massachusetts College of Pharmacy and Health Science" ~ "Massachusetts College of Pharmacy and Health Science (Boston)",
     e_name == "Massasoit Community College" ~ "Massasoit Community College (Canton)",
     e_name == "Gordon-Conwell Theological Seminary" ~ "Gordon-Conwell Theological Seminary (Boston)",
     e_name == "Massachusetts Bay Community College" ~ "Massachusetts Bay Community College (Ashland - Auto Tech Center)",
     e_name == "North Shore Community College (Danvers Campus)"~ "North Shore Community College (Danvers)",
     e_name == "Simmons College" ~"Simmons College (Main Campus)",
     # note Spa Tech Institute enrollment assigned to Ipswich campus point
     e_name == "Spa Tech Institute" ~ "Spa Tech Institute (Ipswich)",
     TRUE ~ e_name))
c_prep <- colleges %>% 
  mutate(id = substr(NCES_ID, 1, 6)) %>% 
  mutate(c_name = ifelse(is.na(CAMPUS)==F, paste0(COLLEGE, " (", CAMPUS, ")"), COLLEGE)) %>% 
  select(c_name, COLLEGE, CAMPUS, id, c_NCES_ID= NCES_ID) 

ec <- e_prep %>% 
    left_join(c_prep, by= c("id"= "id", "e_name"= "c_name")) %>% 
    arrange(id)%>% 
    filter(tot_enroll_19 >0) %>% 
    st_as_sf() %>% 
    select(name = e_name, enrollment= tot_enroll_19, NCES= e_NCES_ID) %>% 
    mutate(id = row_number(),
           type = "higherEd")

# Some campus points missing enrollment are not graduate education opportunities
# some campus points missing enrollment had enrollment assigned to the main campus, as it wasn't reported by campus.
c_no_enrollment <- c_prep %>% 
  anti_join(e_prep,by= c("id"= "id", "c_name"= "e_name"))

mapview(ec, cex = 10, color = "red")+ mapview(c_no_enrollment, cex= 4)

# SAVE DATA ####
st_write(ec, "output/DestinationData.gpkg", "higherEd_PT")

# SAVE DATA FOR CONVEYAL ####
ed_conveyal <- st_read("output/DestinationData.gpkg", "higherEd_PT") %>% 
  rename(weight = enrollment) %>% 
  prep_pt_to_csv_keepID_weight()

ed_csv<- ed_conveyal %>%   
  pt_to_csv("output/higherEd.csv")
