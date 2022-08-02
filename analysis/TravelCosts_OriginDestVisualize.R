library(tidyverse)
library(sf)
library(leaflet)
library(classInt)


cost_lines<- st_read("output/TravelCosts.gpkg", layer = "cost_lines") %>% 
  filter(name_destination %in% c( 4, 26, 44, 16, 1006) & is.na(mean_costTransit)==F) %>% 
  mutate(delta = round(mean_costTransit - cost_drive, 2),
         mean_time = round(mean_time),
         cost_drive = round(cost_drive, 2),
         mean_costTransit = round(mean_costTransit, 2)
         ) %>% 
  arrange(desc(mean_costTransit)) %>% 
  st_transform(4326) %>% 
  mutate(to = case_when(name_destination == 4 ~ "Longwood Medical Area",
                        name_destination == 26 ~ "Downtown Boston",
                        name_destination == 44 ~ "Quincy",
                        name_destination == 16 ~ "Lynn", 
                        name_destination == 1006 ~ "Framingham")) %>% 
  select( -c( min_time, max_time, min_costTransit, max_costTransit))


pts<- st_read("output/TravelCosts.gpkg", layer = "ODpts")

commTypes_byMuni <- st_read("app/data/AggregationAreas.gpkg", "CommunityTypes") %>% st_transform(3857)
munis <- commTypes_byMuni %>% 
  st_transform(4326)

commTypes<- commTypes_byMuni %>%
  group_by(communityType, subtype) %>%
  summarize(geometry = st_union(geom)) %>% 
  st_as_sf() %>% 
  st_transform(4326)

origins <- pts %>% 
  st_transform(4326) %>% 
  filter(type == "o") %>%
  st_intersection(select(munis, municipality, communityType, subType))

test <- cost_lines %>% left_join(select(st_drop_geometry(origins), name, municipality), by = c("name_origin"= "name"))

write_rds(test, "app/data/cost_lines.rds")
cost_data <- cost_lines 

brks <- classIntervals(c(min(cost_data$delta) - .00001,
                         cost_data$delta), n = 5, style = "jenks")

pal_delta<-colorBin(palette = "YlGnBu",domain = cost_data$delta, bins =brks$brks, pretty = FALSE)

labels <- sprintf(
  "Cost delta: <strong>$%.2f</strong><br/></sup>",
  cost_data$delta
) %>% lapply(htmltools::HTML)


delta_map <- leaflet(options = leafletOptions(preferCanvas = TRUE,
                                        minZoom= 8,
                                        maxZoom= 15, 
                                        attributionConrol= FALSE,
                                        closePopupOnClick= FALSE)) %>% 
  setView(lng = -71.059, lat = 42.35, zoom = 10) %>%
  addResetMapButton() %>% 
  addPolygons(data = commTypes,
              color = "white",
              fillColor = "tan",
              weight= .5,
              smoothFactor = .6,
              opacity = 1, 
              fillOpacity = 1,
              popup = paste0("<b>",commTypes$communityType, ": ", commTypes$subtype,"</b><br>")) %>% 
  addPolygons(data = munis,
              color = "white",
              fillColor = "transparent",
              weight= .5,
              smoothFactor = .6,
              opacity = 1, 
              fillOpacity = 1,
              popup = paste0("<b>",munis$municipality, "</b><br>",
                             munis$communityType, ": ", munis$subtype)) %>% 
  addPolylines(data = cost_data,
               weight = cost_data$delta/10,
               color = ~pal_delta(delta),#"green",
               opacity= .8,
              popup = paste0("From: ", "<br>",
                             "To: ",cost_data$to,"<br>",
                             "Cost Delta: $", formatC(round(cost_data$delta,2), format= 'f', digits= 2),"<br>",
                             "Drive Time: ", round(cost_data$drive_time), " minutes <br>",
                             "Drive Cost: $", formatC(round(cost_data$cost_drive,2), format= 'f', digits= 2),"<br>",
                             "Avg Transit Time: ", round(cost_data$mean_time), " minutes <br>",
                             "Transit Cost: $", formatC(round(cost_data$mean_costTransit, 2), format= 'f', digits= 2)),
              # label = paste0("From: ", "<br>",
              #                "To: ", "<br>",
              #                "Cost Delta: $", formatC(round(cost_data$delta,2), format= 'f', digits= 2)),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions =  highlightOptions(
                weight = 5,
                bringToFront = FALSE)) %>%
  addLegend(pal = pal_delta, 
            values =cost_data$delta,
            labFormat = labelFormat(prefix = "$"),
            title = "How much more does <br>transit cost? <br>
            (Cost Delta)",
            position =  "bottomright")
delta_map
