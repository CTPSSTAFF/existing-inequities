#
library(shiny)
#library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(sf)
library(Cairo)
library(stars)
library(reactable)
library(htmltools)
library(classInt)
library(scales)
#library(knitr)
library(leaflet)
library(leaflet.extras)
library(ggiraph)

app_inputs <- read_rds("data/app_inputs.rds")

mpoBoundary <- st_read("data/AggregationAreas.gpkg", "MPO_Boundary") %>%
  st_transform(3857)

comm_types_id<- read_rds("data/comm_types_id.rds")
commTypes_byMuni <- st_read("data/AggregationAreas.gpkg", "CommunityTypes") %>% st_transform(3857)
commTypes<- commTypes_byMuni %>%
  group_by(communityType, subtype) %>%
  summarize(geometry = st_union(geom)) %>% 
  st_as_sf() %>% 
  left_join(comm_types_id)
# higherEd <- st_read("data/DestinationData.gpkg", "higherEd_PT")
# healthcare <- st_read("data/DestinationData.gpkg", "healthcare_PT")
# essentialPlace_poly <- st_read("data/DestinationData.gpkg", "essentialPlace_Final_POLY")
# essentialPlace_pt <- st_read("data/DestinationData.gpkg", "essentialPlace_Final_PT")

healthcare_nonemg_access <- read_rds("data/healthcareNonEmg_access.rds")
healthcare_emg_access <- read_rds("data/healthcareEmg_access.rds")
jobs_access <- read_rds("data/jobs_access.rds")
essentialplaces_access <- read_rds("data/essentialplaces_access.rds")
highered_access <- read_rds("data/highered_access.rds")
openspace_access <- read_rds("data/openspace_access.rds")
openspace_conservation_access<- read_rds("data/openspace_conservation_access.rds")
openspace_paths_access <- read_rds("data/openspace_paths_access.rds")

dasy_raster<- read_rds("data/dasy_raster.rds")
weights_all_for_plot <- read_rds("data/weights_for_all_plot.rds")
comm_types_rast<- read_rds("data/comm_types_rast.rds")

`%notin%` <- Negate(`%in%`)


t <- read_rds("data/hta_index_tracts.rds")
m <- read_rds("data/hta_index_munis.rds")
index_vars <- read_rds("data/hta_index_vars.rds")

access_all_comp <- read_csv("data/access_ratios.csv")

visualize_for_access_w_demo <- function(access, demo, dests,modes, cols = 4 ){
  if (dests == 1) {d <- "Healthcare opportunities, \nnon-emergency"
  time_period <- "AM Peak"}
  if (dests == 2) {d <- "Healthcare opportunities, \nemergency"
  time_period <- "AM Peak"}
  if (dests == 3) {d <- "Jobs"
  time_period = "AM Peak"}
  if (dests == 4) {d <- "Higher Education"
  time_period = "Midday"}
  if (dests == 5) {d <- "Essential Places"
  time_period = "AM Peak" }
  if (dests == 6) {d <- "Open Space, all parks"
  time_period = "Weekend, Midday" }
  if (dests == 7) {d <- "Open Space, large parks"
  time_period = "Weekend, Midday" }
  if (dests == 8) {d <- "Open Space, paths"
  time_period = "Weekend, Midday" }
  
  t <- paste(str_sub(word(names(access), sep = "min"), start = -2), "minute")
  
  if (time_period ==  "AM Peak" ) {
    m <- word(word(names(access), sep = "_..min"), sep = "_.M_", start = 2)
  } else if(time_period ==  "Midday") {
    m <- word(word(names(access), sep = "_..min"), sep = "_MD_", start = 2)
  } else{
    m <- word(word(names(access), sep = "_..min"), sep = "_Weekend_", start = 2)
  }
  m <- str_replace(m, "TransitAll", "Transit")
  
  n <- paste(t, m)
  names(access)<- n
  
  if(demo == 0){
    access_weighted <- access
  } else if (demo == 1){
    weight <-select(weights_all_for_plot, pct_pop)
    access_w <- access*weight
    names(access_w)<- paste("Population weighted,\n", names(access_w))
    access_weighted <- c( access_w)
  } else if (demo == 2){
    weight <-select(weights_all_for_plot, pct_pop_adult)
    access_w <- access*weight
    names(access_w)<- paste("Adult population weighted,\n", names(access_w))
    access_weighted <- c( access_w)
  } else if (demo == 3){
    weight_ej <- select(weights_all_for_plot, pct_min_adult)
    weight_nonej <- select(weights_all_for_plot, pct_nonmin_adult)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Minorirty (Adult),\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Nonminority (Adult),\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else if (demo == 4){
    weight_ej <- select(weights_all_for_plot, pct_min)
    weight_nonej <- select(weights_all_for_plot, pct_nonmin)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Minorirty,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Nonminority,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else if (demo == 5){
    weight_ej <- select(weights_all_for_plot, pct_lowinc_adult)
    weight_nonej <- select(weights_all_for_plot, pct_nonlowinc_adult)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Low-income (Adult),\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-low-income (Adult),\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else if (demo == 6){
    weight_ej <- select(weights_all_for_plot, pct_lowinc)
    weight_nonej <- select(weights_all_for_plot, pct_nonlowinc)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Low-income,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-low-income,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else if (demo == 7){
    weight_ej <- select(weights_all_for_plot, pct_zvhh)
    weight_nonej <- select(weights_all_for_plot, pct_nonzvhh)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Zero-vehicle households,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-zero vehicle households,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else (
    access_weighted <- access
  )
  
  
  plot_start <- function(access_weighted) {
    ggplot()+
      geom_stars(data = access_weighted)+
      geom_sf(data = commTypes_byMuni, size=.2, color = 'light gray', fill = 'transparent')+
      geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",
                          na.value = "transparent",
                          name = "Number of \nopportunities",
                          labels = comma)+
      # facet_wrap(~new_dim, ncol = cols)+
      ggtitle(paste0("Access to \n", d))+
      labs(caption= paste0("Time period: ", time_period))+
      theme_void()+
      theme(text=element_text(size=20),
            plot.title = element_text(hjust = 0.5, size = 30),
            legend.text=element_text(size=11))
  }
  
  if (demo == 0 &length(names(access_weighted))>1 & length(modes)>1){
    # plots when no demo split multiple modes selected AND multiple layers
    access_weighted <- access_weighted %>% st_redimension()
    plot <- plot_start(access_weighted )+
      facet_wrap(~new_dim ) #,ncol = cols
    plot
  }else if (length(names(access_weighted))>1 & length(modes)>1){
    # plots when multiple modes selected AND multiple layers
    access_weighted <- access_weighted %>% st_redimension()
    plot <- plot_start(access_weighted )+
      facet_wrap(~new_dim ,ncol = cols) #,ncol = cols
    plot
  } else if (length(names(access_weighted))>1 & length(modes)==1){
    # plots when single mode selected AND multiple layers
    access_weighted <- access_weighted %>% st_redimension()
    plot <- plot_start(access_weighted )+
      facet_wrap(~new_dim)
    plot
  } else {
    # plots when only one mode and no demo (only one layer to map)
    plot <- plot_start(access_weighted )
    plot
  }
}
visualize_for_access_w_demo_w_tooltip <- function(access, demo, dests,modes, cols = 4 , outside_agg){
  if (dests == 1) {d <- "Healthcare opportunities, \nnon-emergency"
  time_period <- "AM Peak"}
  if (dests == 2) {d <- "Healthcare opportunities, \nemergency"
  time_period <- "AM Peak"}
  if (dests == 3) {d <- "Jobs"
  time_period = "AM Peak"}
  if (dests == 4) {d <- "Higher Education"
  time_period = "Midday"}
  if (dests == 5) {d <- "Essential Places"
  time_period = "AM Peak" }
  if (dests == 6) {d <- "Open Space, all parks"
  time_period = "Weekend, Midday" }
  if (dests == 7) {d <- "Open Space, large parks"
  time_period = "Weekend, Midday" }
  if (dests == 8) {d <- "Open Space, paths"
  time_period = "Weekend, Midday" }
  
  t <- paste(str_sub(word(names(access), sep = "min"), start = -2), "minute")
  
  if (time_period ==  "AM Peak" ) {
    m <- word(word(names(access), sep = "_..min"), sep = "_.M_", start = 2)
  } else if(time_period ==  "Midday") {
    m <- word(word(names(access), sep = "_..min"), sep = "_MD_", start = 2)
  } else{
    m <- word(word(names(access), sep = "_..min"), sep = "_Weekend_", start = 2)
  }
  m <- str_replace(m, "TransitAll", "Transit")
  
  n <- paste(t, m)
  names(access)<- n
  
  if(demo == 0){
    access_weighted <- access
  } else if (demo == 1){
    weight <-select(weights_all_for_plot, pct_pop)
    access_w <- access*weight
    names(access_w)<- paste("Population weighted,\n", names(access_w))
    access_weighted <- c( access_w)
  } else if (demo == 4){
    weight_ej <- select(weights_all_for_plot, pct_min)
    weight_nonej <- select(weights_all_for_plot, pct_nonmin)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Minorirty,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Nonminority,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else if (demo == 6){
    weight_ej <- select(weights_all_for_plot, pct_lowinc)
    weight_nonej <- select(weights_all_for_plot, pct_nonlowinc)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Low-income,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-low-income,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else if (demo == 7){
    weight_ej <- select(weights_all_for_plot, pct_zvhh)
    weight_nonej <- select(weights_all_for_plot, pct_nonzvhh)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Zero-vehicle households,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-zero vehicle households,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else (
    access_weighted <- access
  )
  
  
  plot_start <- function(access_weighted) {
    ggplot()+
      geom_stars(data = access_weighted)+
      #geom_sf(data = outside_agg, size=0, fill = '#F2F2F2', color = "transparent")+
      geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
      geom_sf_interactive(data = commTypes_byMuni, size=.2, 
                          color = 'light gray',
                          fill = 'transparent', #'pink',
                          aes(tooltip = municipality, data_id = municipality))+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",
                          na.value = "transparent",
                          name = "Number of \nopportunities",
                          labels = comma)+
      # facet_wrap(~new_dim, ncol = cols)+
      ggtitle(paste0("Access to \n", d),
              subtitle = " ")+
      labs(caption= paste0("Time period: ", time_period))+
      theme_void()+
      theme(text=element_text(size=11, family = 'sans'),
            plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, face = "bold"),
            legend.text=element_text(size=10),
            strip.text = element_text(face = "bold", size = 10),
            strip.placement = "inside") #legend.position = "bottom")
  }
  
  if (demo == 0 &length(names(access_weighted))>1 & length(modes)>1){
    # plots when no demo split multiple modes selected AND multiple layers
    access_weighted <- access_weighted %>% st_redimension()
    plot <- plot_start(access_weighted )+
      facet_wrap(~new_dim ) #,ncol = cols
  }else if (length(names(access_weighted))>1 & length(modes)>1){
    # plots when multiple modes selected AND multiple layers
    access_weighted <- access_weighted %>% st_redimension()
    plot <- plot_start(access_weighted )+
      facet_wrap(~new_dim ,ncol = cols) #,ncol = cols
  } else if (length(names(access_weighted))>1 & length(modes)==1){
    # plots when single mode selected AND multiple layers
    access_weighted <- access_weighted %>% st_redimension()
    plot <- plot_start(access_weighted )+
      facet_wrap(~new_dim)
  } else {
    # plots when only one mode and no demo (only one layer to map)
    name <- names(access_weighted)
    plot <- plot_start(access_weighted )+labs(subtitle = name)
    
  }
  return(plot)
}

shinyServer(function(input, output, session) {
  # access_rv <- reactiveValues()
  # access_rv$access <- NULL
  # access_rv$access_mpo <- NULL
  
  observeEvent(input$dest,{
  mt_choices <- app_inputs %>% filter(dest_id == input$dest )
  choice_list <-mt_choices$mt_id
  names(choice_list)<- mt_choices$mode_time
  updatePickerInput(session = session, inputId = "mode_time",
                    choices = choice_list, selected = c(1,2))
  })
  
  output$access_plots <- renderGirafe({
    dests <- input$dest
    mts <- app_inputs %>% filter(dest_id == dests)%>% filter(mt_id %in% input$mode_time)
    modes <- mts$mode_id#input$modes
    time <- mts$time_id#input$time
    agg <- input$aggArea
    demo <- as.numeric(input$demo)
    if (dests == 1) {access <- healthcare_nonemg_access}
    if (dests == 2) {access <- healthcare_emg_access}
    if (dests == 3) {access <- jobs_access}
    if (dests == 4) {access <- highered_access}
    if (dests == 5) {access <- essentialplaces_access}
    if (dests == 6) {access <- openspace_access}
    if (dests == 7) {access <- openspace_conservation_access}
    if (dests == 8) {access <- openspace_paths_access}
    
    if (1 %notin% modes) { access <- access %>% select(-contains("Walk"))}
    if (2 %notin% modes) { access <- access %>% select(-contains("Bike"))}
    if (3 %notin% modes) { access <- access %>% select(-contains("TransitBusRT"))}
    if (4 %notin% modes) { access <- access %>% select(-contains("TransitAll"))}
    if (5 %notin% modes) { access <- access %>% select(-contains("Drive"))}

    if (1 %notin% time) { access <- access %>% select(-ends_with("15min"))}
    if (2 %notin% time) { access <- access %>% select(-ends_with("30min"))}
    if (3 %notin% time) { access <- access %>% select(-ends_with("45min"))}
    if (4 %notin% time) { access <- access %>% select(-ends_with("60min"))}

    # access_rv$access_mpo <- access

    if (agg == 1) { access <- access * (comm_types_rast %>% select(id1))
    outside_agg <- commTypes %>% filter(id != 1)
    }
    if (agg == 2) { access <- access * (comm_types_rast %>% select(id2))
    outside_agg <- commTypes %>% filter(id != 2)}
    if (agg == 3) { access <- access * (comm_types_rast %>% select(id3))
    outside_agg <- commTypes %>% filter(id != 3)}
    if (agg == 4) { access <- access * (comm_types_rast %>% select(id4))
    outside_agg <- commTypes %>% filter(id != 4)}
    if (agg == 5) { access <- access * (comm_types_rast %>% select(id5))
    outside_agg <- commTypes %>% filter(id != 5)}
    if (agg == 6) { access <- access * (comm_types_rast %>% select(id6))
    outside_agg <- commTypes %>% filter(id != 6)}
    if (agg == 7) { access <- access * (comm_types_rast %>% select(id7))
    outside_agg <- commTypes %>% filter(id != 7)}
    if (agg == 8) {
      outside_agg <- commTypes %>% filter(id == 8)
    }
    
    # access_rv$access <- access
    # test_access <<-access
    # test_demo <<- demo
    # test_dests <<- dests
    # test_modes <<- modes
    # test_cols <<- length(modes)
    # test_outside_agg <<- outside_agg
    
    plot <- visualize_for_access_w_demo_w_tooltip(access,demo,dests,modes, cols= length(modes), outside_agg) 
    girafe(ggobj = plot,
           width_svg = 12, height_svg = 7,
           options = list(
             opts_selection(type= "none"),
             opts_sizing(rescale = TRUE, width = 1) ,
             opts_hover(css = "fill:gray !important ;fill-opacity: .7; stroke:darkgray !important; ")))
    
  })
  
  output$avgs<- renderReactable({
    dests <- input$dest
    mts <- app_inputs %>% filter(dest_id == dests)%>% filter(mt_id %in% input$mode_time)
    modes <- mts$mode_id#input$modes
    time <- mts$time_id#input$time
    agg <- input$aggArea
    demo <- as.numeric(input$demo)
    
    if (dests == 1) {d <-  "Healthcare, Non-emergency" }
    if (dests == 2) {d <- "Healthcare, Emergency"}
    if (dests == 3) {d <- "Jobs"  }
    if (dests == 4) {d <- "Higher Education" }
    if (dests == 5) {d <-  "Essential Places" }
    if (dests == 6) {d <- "Open Space"   }
    if (dests == 7) {d <- "Open Space, Conservation" }
    if (dests == 8) {d <- "Open Space, Paths"}
    
    m <- c()
    if (1 %in% modes) { m <- c(m, "Walk")}
    if (2 %in% modes) { m <- c(m, "Bike")}
    if (3 %in% modes) { m <- c(m, "Transit (Bus and RT only)")}
    if (4 %in% modes) { m <- c(m, "Transit (All modes)")}
    if (5 %in% modes) { m <- c(m, "Drive")}
    
    t <- c()
    if (1 %in% time) { t<- c(t, 15)}
    if (2 %in% time) { t<- c(t, 30)}
    if (3 %in% time) { t<- c(t, 45)}
    if (4 %in% time) { t<- c(t, 60)}
    
    a <- c()
    if (agg == 1) { a <- c(a, "Developing Suburbs: Maturing New England Towns")}
    if (agg == 2) { a <- c(a, "Inner Core: Streetcar Suburbs")}
    if (agg == 3) { a <- c(a, "Developing Suburbs: Country Suburbs")}
    if (agg == 4) { a <- c(a, "Maturing Suburbs: Established Suburbs and Cape Cod Towns")}
    if (agg == 5) { a <- c(a, "Maturing Suburbs: Mature Suburban Towns")}
    if (agg == 6) { a <- c(a, "Inner Core: Metro Core Communities")}
    if (agg == 7) { a <- c(a, "Regional Urban Centers: Sub-Regional Urban Centers")}
    if (agg == 8) { a <- c(a, "MPO")}
    
    dem <- c()
    if (0 %in% demo) { dem <- c(dem, "Total population, adult", "Total population" , "Total households" )}
    if (3 %in% demo) { dem <- c(dem, "Minority Status, adult", "Total population, adult")}
    if (4 %in% demo) { dem <- c(dem, "Minority Status","Total population"  )}
    if (5 %in% demo) { dem <- c(dem, "Income status, adult", "Total population, adult")}
    if (6 %in% demo) { dem <- c(dem, "Income status", "Total population")}
    if (7 %in% demo) { dem <- c(dem, "Household vehicles" ,"Total households"   )}
    
    access_tbl <- access_all_comp %>% 
      filter( destination == d) %>% 
      filter(mode %in% m) %>%
      filter(time %in% t) %>% 
      filter(region %in% a) %>% 
      filter(type %in% dem) %>% 
      rename(`Equity Population `= type,
             `Destinations` = destination,
             `Mode` = mode,
             `Travel Time (minutes)`= time,
             `Aggregation Area `=  region,
             `Total Population`= Total,
             `Equity Population` = EJ,
             `Non-Equity Popultation`= `Non-EJ`,
             `Aggregation Area`= `Ratio Aggregation Area`,
             `MPO Region`= `Ratio MPO`)
    reactable(access_tbl, compact = TRUE, 
              #filterable = T,
              highlight = T, 
              # searchable = T
              # groupBy = c("Destinations", "Mode", "Travel Time (minutes)"),
              columns = list(
                `Destinations`= colDef(minWidth = 150),
                `Aggregation Area ` = colDef(minWidth = 200),
                `Travel Time (minutes)`= colDef(format = colFormat(suffix = " min")),
                `Total Population` = colDef(filterable = F,  format = colFormat(separators = T)),
                `Equity Population` = colDef(filterable = F, format = colFormat(separators = T)),
                `Non-Equity Popultation` = colDef(filterable  = F, format = colFormat(separators = T)),
                `Aggregation Area`=  colDef(filterable =F,
                                            # count inequity flags
                                            aggregate = JS("function(values, rows) {
        let flag = 0
        rows.forEach(function(row) {
         if ( row['Aggregation Area'] < 1 ) {
          flag += 1
          }
        })
        return( flag + ' equity flags')
      }"),
                                            style = JS("function(rowInfo) {
      const value = rowInfo.values['Aggregation Area']
      let color
      let weight
      if (value < 1) {
        color = '#e00000'
        weight =  'bold'
      } else {
        color = '#000'
        weight= 'normal'
      }
      return { color: color, fontWeight: weight }
    }")
                ),
                `MPO Region`= colDef(filterable= F,
                                     # count inequity flags
                                     aggregate = JS("function(values, rows) {
        let flag = 0
        rows.forEach(function(row) {
         if ( row['MPO Region'] < 1 ) {
          flag += 1
          }
        })
        return( flag + ' equity flags')
      }"),
                                     style = JS("function(rowInfo) {
      const value = rowInfo.values['MPO Region']
      let color
      let weight
      if (value < 1) {
        color = '#e00000'
        weight= 'bold'
      } else {
        color = '#000'
        weight = 'normal'
      }
      return { color: color, fontWeight: weight }
    }"))),
              columnGroups = list(
                colGroup(name = "Average Number of Opportunities Accessible Per Person, by Demographic",
                         columns= c("Total Population", "Equity Population", "Non-Equity Popultation")),
                colGroup(name = "Equity Check Ratio",
                         columns= c("Aggregation Area", "MPO Region")))
    )
    
  })
  
  output$access_all <- renderReactable({
    access_all_tbl <- access_all_comp %>% 
      mutate(mode = ifelse(mode == "Transit (All modes)", "Transit", mode)) %>% 
      rename(`Equity Population `= type,
             `Destinations` = destination,
             `Mode` = mode,
             `Travel Time (minutes)`= time,
             `Aggregation Area `=  region,
             `Total Population`= Total,
             `Equity Population` = EJ,
             `Non-Equity Popultation`= `Non-EJ`,
             `Aggregation Area`= `Ratio Aggregation Area`,
             `MPO Region`= `Ratio MPO`)
    reactable(access_all_tbl, compact = TRUE, 
              #filterable = T,
              highlight = T, 
             # searchable = T
             groupBy = c("Destinations", 'Aggregation Area '),
             columns = list(
               `Destinations`= colDef(minWidth = 150),
               `Aggregation Area ` = colDef(minWidth = 200),
               `Mode` = colDef(aggregate = "unique"),
               # `Equity Population ` = colDef(aggregate = "unique"),
               `Travel Time (minutes)`= colDef(format = colFormat(suffix = " min"),
                                               aggregate = "unique"),
               `Total Population` = colDef(  format = colFormat(separators = T)),
               `Equity Population` = colDef(filterable = F, format = colFormat(separators = T)),
               `Non-Equity Popultation` = colDef(filterable  = F, format = colFormat(separators = T)),
               `Aggregation Area`=  colDef(filterable =F,
                                                 # count inequity flags
                                                 aggregate = JS("function(values, rows) {
        let flag = 0
        rows.forEach(function(row) {
         if ( row['Aggregation Area'] < 1 ) {
          flag += 1
          }
        })
        let unit = ''
        if (flag === 1) { unit = ' equity flag'} else {unit = ' equity flags'}
        return( flag + unit)
      }"),
                                                 style = JS("function(rowInfo) {
      const value = rowInfo.values['Aggregation Area']
      let color
      let weight
      if (value < 1) {
        color = '#e00000'
        weight =  'bold'
      } else {
        color = '#000'
        weight= 'normal'
      }
      return { color: color, fontWeight: weight }
    }")
                                                 ),
               `MPO Region`= colDef(filterable= F,
                                   # count inequity flags
                                   aggregate = JS("function(values, rows) {
        let flag = 0
        rows.forEach(function(row) {
         if ( row['MPO Region'] < 1 ) {
          flag += 1
          }
        })
        let unit = ''
        if (flag === 1) { unit = ' equity flag'} else {unit = ' equity flags'}
        return( flag + unit)
      }"),
                                   style = JS("function(rowInfo) {
      const value = rowInfo.values['MPO Region']
      let color
      let weight
      if (value < 1) {
        color = '#e00000'
        weight= 'bold'
      } else {
        color = '#000'
        weight = 'normal'
      }
      return { color: color, fontWeight: weight }
    }"))),
             columnGroups = list(
               colGroup(name = "Average Number of Opportunities Accessible Per Person, by Demographic",
                        columns= c("Total Population", "Equity Population", "Non-Equity Popultation")),
               colGroup(name = "Equity Check Ratio",
                        columns= c("Aggregation Area", "MPO Region")))
             )
  })
  
  output$index_plot <- renderPlot({
    bound <- mpoBoundary %>% st_transform(26986)
    bb <- st_bbox(bound)
    munis <- commTypes_byMuni %>% st_transform(26986)
    trcts3 <- trcts %>% 
      select(var = ht_ami)
    name <- vars["ht_ami"]
    brks <- classIntervals(c(min(trcts3$var) - .00001,
                             trcts3$var), n = 5, style = "jenks")
    trcts3 <- trcts3 %>% 
      mutate( var_cat = cut(var, brks$brks)) 
    
    ggplot()+
      geom_sf(data= trcts3,color = "white", size =.1, aes(fill = var_cat))+
      geom_sf(data= munis , color = "white", size = .2, fill = "transparent")+
      #geom_sf(data = mpoBoundary, color = "pink", size = 1, fill = "transparent")+
      scale_fill_brewer(palette = "YlGnBu", name = str_wrap(name, width = 10))+
      coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]))+
      theme_void()
  })
  
  output$index_map <- renderLeaflet({
    t_m <- t %>% mutate(v = ht_ami)
    pal <- colorBin("YlGnBu", t_m$v, 5, pretty = F, na.color = "white")
    map <- leaflet(options = leafletOptions(preferCanvas = TRUE,
                                            minZoom= 8,
                                            maxZoom= 15, 
                                            attributionConrol= FALSE,
                                            closePopupOnClick= FALSE)) %>% 
      setView(lng = -71.059, lat = 42.35, zoom = 10) %>%
      addResetMapButton() %>%
      addPolygons(data = t_m,
                  group = "map data",
                  color = "white",
                  weight= .5,
                  smoothFactor = .6,
                  opacity = 1, 
                  fillOpacity = 1,
                  fillColor = ~pal(v)) %>% 
      addLegend(position = "bottomright",
                title= "ht_ami",
                group = "map data",
                pal = pal, values = t_m$v,
                opacity = 1) %>% 
      addPolygons(data = m,
                  color = "white",
                  weight = 1,
                  smoothFactor = .6,
                  fillColor = "tranparent",
                  fillOpacity = 0) 
  })
  
  observe({
    var <- input$index_var
    # legend_name <- unname(index_vars[var])
    # legend_title <- str_replace_all(str_wrap(unname(legend_name), width =10), "\n", "<br>")
    t_map <- t %>% 
      mutate(v = case_when(
        var == "t_cost_ami" ~ t_cost_ami,
        var == "h_cost" ~ h_cost,
        var == "h_ami" ~ h_ami,
        var == "t_ami" ~ t_ami,
        var == "ht_ami" ~ ht_ami,
        var == "auto_ownership_cost_ami" ~ auto_ownership_cost_ami,
        var == "transit_cost_ami" ~ transit_cost_ami
      ))
    pal <- colorBin("YlGnBu", t_map$v, 5, pretty = F, na.color = "white")
    
    leafletProxy("index_map") %>% 
      clearControls() %>%
      clearGroup(group= "map data" )%>%
      addPolygons(data = t_map,
                  group = "map data",
                  color = "white",
                  weight= .5,
                  smoothFactor = .6,
                  opacity = 1, 
                  fillOpacity = 1,
                  fillColor = ~pal(v)) %>% 
      addLegend(position = "bottomright",
                title= var,
                group = "map data",
                pal = pal, values = t_map$v,
                opacity = 1)
  })


})
