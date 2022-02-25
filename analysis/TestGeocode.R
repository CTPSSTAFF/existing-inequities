# testing tidygeocode



# Tidygeocoder has a methods argument, 
# Tried using Open Street Map's Nominatim, but results were not as clean as method = "arcgis"
# When method = "arcgis" results are analogous to runnging addresses through ArcGIS geoproccessing
# https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html

# Prep non-spatial data for geocoding
test <- CommunityHealthCenters_Oct2019 %>% 
  select(name= site_name, addr_st= address, town= mail_city, zip = zip) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
  select(name, address)

test_coded <- test %>% 
  geocode(address, method= 'osm', lat = latitude , long = longitude)
test_coded_sf<- test_coded %>% 
  filter(is.na(latitude)==F) %>% 
  st_as_sf(coords= c("longitude", "latitude"), crs=4326 )
st_write(test_coded_sf,"output_test/test_geocode.gpkg", "test_code1")


test_coded2<- test %>% 
  geocode(address, method= 'arcgis', lat = latitude , long = longitude)

test_coded_sf2<- test_coded2 %>% 
  filter(is.na(latitude)==F) %>% 
  st_as_sf(coords= c("longitude", "latitude"), crs=4326 )
st_write(test_coded_sf2,"output_test/test_geocode2.gpkg", "test_code2")

mapview(comp, color= "green", cex= 3)+ mapview(test_coded_sf2, color= "pink", cex= 2)
comp <- CommunityHealthCenters_Oct2019 %>% 
  select(name= site_name, addr_st= address, town= mail_city, zip = zip, shape) %>% 
  mutate(address= paste0(addr_st,", ", town, ", MA ",zip )) %>% 
  select(name, address, shape) %>% 
  rowwise() %>% 
  mutate(x = substring(str_split(shape, " ")[[1]][2],2),
         y = substring(str_split(shape, " ")[[1]][3],1,nchar(str_split(shape, " ")[[1]][3])-1)) %>%
  select(-shape) %>% 
  st_as_sf(coords= c("x", "y"), crs= 26986) %>% 
  st_transform(4326) %>% 
  st_as_sf()
st_write(comp,"output_test/test_geocode2.gpkg", "comp")



# 
test <- read_sf("http://gisprpxy.itd.state.ma.us/arcgisserver/rest/services/AGOL/MassGIS_Master_Address_Points/MapServer/json")
