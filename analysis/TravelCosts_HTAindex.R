
library(classInt)

# cost data from Center for Neighborhood Technology
costs_tract <- read_csv("data/TravelCost/htaindex_data_tracts_25.csv") %>% 
  mutate(tract= str_sub(tract,start =2, end= -2))
# https://htaindex.cnt.org/download/data-dictionary.php

vars <- c(t_cost_ami = 'Annual Transportation Cost for the Regional Typical Household',
          h_cost = 'Average Monthly Housing Cost',
          h_ami = 'Housing Costs % Income for the Regional Typical Household',
          t_ami = 'Transportation Costs % Income for the Regional Typical Household',
          ht_ami = 'Housing + Transportation Costs % Income for the Regional Typical Household',
          auto_ownership_cost_ami  = 'Annual Auto Ownership Cost for the Regional Typical Household',
          transit_cost_ami = 'Annual Transit Cost for the Regional Typical Household')

vars
names <- unname(vars)
vals <- names(vars)
names(vals)<- names

tracts <- tidycensus::get_decennial(geography = "tract",
                                   variables = "H010001",
                                   year = 2010,
                                   state = "MA",
                                   geometry = T)
munis <- tidycensus::get_decennial(geography= "county subdivision",
                                   variables = "H010001",
                                   year = 2010,
                                   state = "MA",
                                   geometry = T)
br_mpo_munis <- read_csv("data/town_codes.csv") %>%
  filter(MPO == "Boston") %>%
  mutate(GEOID10 = paste0(STATE, str_pad(COUNTY10, 3, "left", "0"), str_pad(COUSUB10, 5, "left", "0")))
munis2 <- munis %>% filter(GEOID %in% br_mpo_munis$GEOID10) %>% 
  st_transform(26986)

mpoBoundary <- st_read("app/data/AggregationAreas.gpkg", "MPO_Boundary") 
bb <- st_bbox(mpoBoundary)


tracts2 <- tracts %>% 
  select(GEOID, popDec10 = value) %>% 
  left_join(costs_tract, by = c("GEOID"= "tract")) %>% 
  st_transform(26986) 
t2141 <- tracts2%>%
  filter(GEOID == "25009214100")
tracts2 <- tracts2 %>%   
  st_filter(st_union(munis2), .predicate = st_within) %>% 
  bind_rows(t2141) %>% 
  st_as_sf() %>%
  select(GEOID, t_cost_ami, h_cost, h_ami, t_ami, ht_ami, auto_ownership_cost_ami, transit_cost_ami)

#write_rds(tracts2, "app/data/hta_index_tracts.rds")
#write_rds(vars, "app/data/hta_index_vars.rds")

tracts_classed <- tracts2 %>% 
  select(var = transit_cost_ami)
name <- vars["transit_cost_ami"]
brks <- classIntervals(c(min(tracts_classed$var) - .00001,
                         tracts_classed$var), n = 5, style = "jenks")
tracts_classed <- tracts_classed %>% 
  mutate( var_cat = cut(var, brks$brks)) 

ggplot()+
  geom_sf(data= tracts_classed,color = "white", size =.1, aes(fill = var_cat))+
  geom_sf(data= munis2, color = "white", size = .2, fill = "transparent")+
  geom_sf(data = mpoBoundary, color = "pink", size = 1, fill = "transparent")+
  scale_fill_brewer(palette = "YlGnBu", name = str_wrap(name, width = 10))+
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]))+
  theme_void()

t <- tracts2%>% st_transform(4326)
m <- munis2 %>% st_transform(4326)

write_rds(t, "app/data/hta_index_tracts.rds")
write_rds(m, "app/data/hta_index_munis.rds")
write_rds(vals, "app/data/hta_index_vars.rds")

library(leaflet)

pal <- colorBin("YlGnBu", t$ht_ami, 5, pretty = F, na.color = "white",)
leaflet() %>% 
  addPolygons(data = t,
              color = "white",
              weight= .5,
              smoothFactor = .6,
              opacity = 1, 
              fillOpacity = 1,
              fillColor = ~pal(ht_ami),
              popup = paste0()
              
              ) %>% 
  addPolygons(data = m,
              color = "white",
              weight = 1,
              smoothFactor = .6, 
              fillColor = "tranparent") %>% 
  addPolygons(data = t,
              color = "transparent",
              weight= .5,
              smoothFactor = .6,
              opacity = 1, 
              fillOpacity = 1,
              fillColor ="transparent",
              popup = paste0(t$GEOID, "<br>",
                             t$ht_ami)
              
  ) %>% 
  addLegend(position = "bottomright",
            pal = pal, values = t$ht_ami,
            opacity = 1)


  # select(`Annual Transportation Cost for the Regional Typical Household` = t_cost_ami ,
  #       `Average Monthly Housing Cost` = h_cost,
  #       # t_ami = 'Transportation Costs % Income for the Regional Typical Household'
  #        # h_ami = 'Housing Costs % Income for the Regional Typical Household',
  #        # t_ami = 'Transportation Costs % Income for the Regional Typical Household',
  #        # ht_ami = 'Housing + Transportation Costs % Income for the Regional Typical Household',
  #        # auto_ownership_cost_ami  = 'Annual Auto Ownership Cost for the Regional Typical Household',
  #        # transit_cost_ami = 'Annual Transit Cost for the Regional Typical Household'
  #       )

# 
# blkgs3 <- blkgs2 %>% 
#   select(var = auto_ownership_cost_ami)
# 
# brks <- classIntervals(c(min(blkgs3$var) - .00001,
#                          blkgs3$var), n = 5, style = "fisher")
# blkgs3 <- blkgs3 %>% 
#   mutate( var_cat = cut(var, brks$brks)) 
#   
# ggplot()+
#   geom_sf(data= blkgs3,color = "white", size =.1, aes(fill = var_cat))+
#   geom_sf(data= munis2, color = "white", size = .2, fill = "transparent")+
#   geom_sf(data = mpoBoundary, color = "pink", size = 1, fill = "transparent")+
#   scale_fill_brewer(palette = "YlGnBu", name = "")+
#   coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]))+
#   theme_void()



