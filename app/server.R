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
#library(knitr)

mpoBoundary <- st_read("data/AggregationAreas.gpkg", "MPO_Boundary") %>%
  st_transform(3857)
commTypes_byMuni <- st_read("data/AggregationAreas.gpkg", "CommunityTypes") %>% st_transform(3857)
# commTypes<- commTypes_byMuni %>% 
#   group_by(communityType) %>% 
#   summarize(geometry = st_union(geom))
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
comm_types_rast<- read_rds("data/comm_types_rast.rds")
comm_types_id<- read_rds("data/comm_types_id.rds")

`%notin%` <- Negate(`%in%`)
total_pop_dec <- sum(as.vector(dasy_raster$pop_dec), na.rm = T)
total_pop_adult <- sum(as.vector(dasy_raster$pop_dec_adult), na.rm = T)
weights_all_for_plot <- dasy_raster %>% 
  mutate(pct_min = minority/(minority+nonminority),
         pct_nonmin = nonminority/(minority+nonminority),
         pct_min_adult = minority_adult/(minority_adult+nonminority_adult),
         pct_nonmin_adult = nonminority_adult/(minority_adult+nonminority_adult),
         pct_lowinc = lowinc/ (lowinc+ nonlowinc),
         pct_nonlowinc = nonlowinc/ (lowinc +nonlowinc),
         pct_lowinc_adult = lowinc_adult/ (lowinc_adult+ nonlowinc_adult),
         pct_nonlowinc_adult = nonlowinc_adult/ (lowinc_adult +nonlowinc_adult),
         pct_zvhh = zero_veh_hh/(zero_veh_hh+ non_zero_veh_hh),
         pct_nonzvhh = non_zero_veh_hh/(zero_veh_hh+ non_zero_veh_hh),
         pct_pop = pop_dec/total_pop_dec,
        pct_pop_adult = pop_dec_adult/ total_pop_adult) %>% 
  select(starts_with("pct"))

trcts <- read_rds("data/hta_index_tracts.rds")
vars <- read_rds("data/hta_index_vars.rds")

visualize_for_access_w_demo <- function(access, demo, dests,modes, cols = 4 ){
  if (dests == 1) {d <- "Healthcare opportunities, Non-emergency"
  time_period <- "AM Peak"}
  if (dests == 2) {d <- "Healthcare opportunities, Emergency"
  time_period <- "AM Peak"}
  if (dests == 3) {d <- "Jobs"
  time_period = "AM Peak"}
  if (dests == 4) {d <- "Higher Education"
  time_period = "Midday"}
  if (dests == 5) {d <- "Essential Places"
  time_period = "AM Peak" }
  if (dests == 6) {d <- "Open Space"
  time_period = "Weekend, Midday" }
  if (dests == 7) {d <- "Open Space, Conservation"
  time_period = "Weekend, Midday" }
  if (dests == 8) {d <- "Open Space, Paths"
  time_period = "Weekend, Midday" }
  
  t <- paste(str_sub(word(names(access), sep = "min"), start = -2), "minute")
  
  if (time_period ==  "AM Peak") {
    m <- word(word(names(access), sep = "_..min"), sep = "_.M_", start = 2)
  } else if(time_period ==  "Midday") {
    m <- word(word(names(access), sep = "_..min"), sep = "_Midday_", start = 2)
  } else{
    m <- word(word(names(access), sep = "_..min"), sep = "_Weekend_", start = 2)
  }
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
    names(access_ej)<- paste("Zero vehicle households,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-zero vehicle households,\n", names(access_nonej))
    access_weighted <- c( access_ej, access_nonej)
  } else (
    access_weighted <- access
  )
  
  
  
  
  if (length(names(access_weighted))>1 & length(modes)>1){
    access_weighted <- access_weighted %>% st_redimension()
    ggplot()+
      geom_stars(data = access_weighted)+
      geom_sf(data = commTypes_byMuni, size=.2, color = 'light gray', fill = 'transparent')+
      geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt", #'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
      facet_wrap(~new_dim, ncol = cols)+
      ggtitle(paste0("Access to ", d))+
      labs(caption= paste0("Time period: ", time_period))+
      theme_void()
  } else if (length(names(access_weighted))>1 & length(modes)==1){
    access_weighted <- access_weighted %>% st_redimension()
    ggplot()+
      geom_stars(data = access_weighted)+
      geom_sf(data = commTypes_byMuni, size=.2, color = 'light gray', fill = 'transparent')+
      geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt", #'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
      facet_wrap(~new_dim)+
      ggtitle(paste0("Access to ", d))+
      labs(caption= paste0("Time period: ", time_period))+
      theme_void()
  } else {
    ggplot()+
      geom_stars(data = access_weighted)+
      geom_sf(data = commTypes_byMuni, size=.3, color = 'light gray', fill = 'transparent')+
      geom_sf(data=mpoBoundary,size=.4,color="light gray", fill= "transparent")+
      coord_sf()+
      scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",#trans= 'log1p',
                          na.value = "transparent",
                          name = "Opportunities Accessible")+
      # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
      ggtitle(paste0("Access to ", d, "\n", names(access)))+
      labs(caption= paste0("Time period: ", time_period))+
      theme_void()
  }
}

shinyServer(function(input, output, session) {
  access_rv <- reactiveValues()
  access_rv$access <- NULL
  access_rv$access_mpo <- NULL
  
  output$access_plots <- renderPlot({
    dests <- input$dest
    modes <- input$modes
    time <- input$time
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

    access_rv$access_mpo <- access

    if (agg == 1) { access <- access * (comm_types_rast %>% select(id1))}
    if (agg == 2) { access <- access * (comm_types_rast %>% select(id2))}
    if (agg == 3) { access <- access * (comm_types_rast %>% select(id3))}
    if (agg == 4) { access <- access * (comm_types_rast %>% select(id4))}
    if (agg == 5) { access <- access * (comm_types_rast %>% select(id5))}
    if (agg == 6) { access <- access * (comm_types_rast %>% select(id6))}
    if (agg == 7) { access <- access * (comm_types_rast %>% select(id7))}
    
    access_rv$access <- access
    plot <- visualize_for_access_w_demo(access,demo,dests,modes, cols= length(modes)) 
    plot
    
  })
  
  output$avgs<- renderReactable({
    dests <- input$dest
    modes <- input$modes
    time <- input$time
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
      rename(`Population Group`= type,
             `Destination` = destination,
             `Mode` = mode,
             `Travel Time (minutes)`= time,
             `Aggregation Area`=  region)
    reactable(access_tbl, compact = TRUE, 
              highlight = T, 
              columns = list(
                `Destination`= colDef(minWidth = 150),
                `Aggregation Area` = colDef(minWidth = 200),
                `Travel Time (minutes)`= colDef(format = colFormat(suffix = " min")),
                Total = colDef(filterable = F,  format = colFormat(separators = T)),
                EJ = colDef(filterable = F, format = colFormat(separators = T)),
                `Non-EJ` = colDef(filterable  = F, format = colFormat(separators = T)),
                `Ratio Aggregation Area`=  colDef(filterable =F,
                                                  style = JS("function(rowInfo) {
      const value = rowInfo.values['Ratio Aggregation Area']
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
                `Ratio MPO`= colDef(filterable= F,
                                    # count inequity flags
                                    style = JS("function(rowInfo) {
      const value = rowInfo.values['Ratio MPO']
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
                colGroup(name = "Average opportunities accessible per person, by population",
                         columns= c("Total", "EJ", "Non-EJ")))
    )
  })
  
  output$access_all <- renderReactable({
    access_all_tbl <- access_all_comp %>% 
      rename(`Population Group`= type,
             `Destination` = destination,
             `Mode` = mode,
             `Travel Time (minutes)`= time,
             `Aggregation Area`=  region)
    reactable(access_all_tbl, compact = TRUE, 
              filterable = T,
              highlight = T, 
             # searchable = T
             groupBy = c("Destination", "Mode", "Travel Time (minutes)"),
             columns = list(
               `Destination`= colDef(minWidth = 150),
               `Aggregation Area` = colDef(minWidth = 200,
                                           # cell = function(value){
                                           #   if(value == "MPO"){
                                           #     #img_src <- knitr::image_uri(sprintf('ct%s.png', 8))
                                           #     img_src <- sprintf("ct%s.png", 8)
                                           #   } else{
                                           #     typ <- word(value, 1,sep = ": ")
                                           #     sub <- word(value, 2, sep = ": ")
                                           #     id <- comm_types_id[comm_types_id$communityType==typ & comm_types_id$subtype==sub,]$id[[1]]
                                           #      # img_src <- knitr::image_uri(sprintf('ct%s.png', id))
                                           #      img_src <- sprintf("ct%s.png", id)
                                           #   }
                                           #   image <- img(src= img_src,
                                           #                style = "height: 24px;",
                                           #                align= "center",
                                           #                alt = value)
                                           #   tagList(
                                           #     value,
                                           #     div(style = "display: inline-block; width: 24px;", image)
                                           #   )
                                           # }
                                           ),
               `Travel Time (minutes)`= colDef(format = colFormat(suffix = " min")),
               Total = colDef(filterable = F,  format = colFormat(separators = T)),
               EJ = colDef(filterable = F, format = colFormat(separators = T)),
               `Non-EJ` = colDef(filterable  = F, format = colFormat(separators = T)),
               `Ratio Aggregation Area`=  colDef(filterable =F,
                                                 # count inequity flags
                                                 aggregate = JS("function(values, rows) {
        let flag = 0
        rows.forEach(function(row) {
         if ( row['Ratio Aggregation Area'] < 1 ) {
          flag += 1
          }
        })
        return( flag + ' equity flags')
      }"),
                                                 style = JS("function(rowInfo) {
      const value = rowInfo.values['Ratio Aggregation Area']
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
               `Ratio MPO`= colDef(filterable= F,
                                   # count inequity flags
                                   aggregate = JS("function(values, rows) {
        let flag = 0
        rows.forEach(function(row) {
         if ( row['Ratio MPO'] < 1 ) {
          flag += 1
          }
        })
        return( flag + ' equity flags')
      }"),
                                   style = JS("function(rowInfo) {
      const value = rowInfo.values['Ratio MPO']
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
               colGroup(name = "Average opportunities accessible per person, by population",
                        columns= c("Total", "EJ", "Non-EJ")))
             )
  })
  
  output$index_plot <- renderPlot({
    
    trcts3 <- trcts %>% 
      select(var = ht_ami)
    name <- vars["ht_ami"]
    brks <- classIntervals(c(min(trcts3$var) - .00001,
                             trcts3$var), n = 5, style = "jenks")
    trcts3 <- trcts3 %>% 
      mutate( var_cat = cut(var, brks$brks)) 
    
    ggplot()+
      geom_sf(data= trcts3,color = "white", size =.1, aes(fill = var_cat))+
      geom_sf(data= munis2, color = "white", size = .2, fill = "transparent")+
      #geom_sf(data = mpoBoundary, color = "pink", size = 1, fill = "transparent")+
      scale_fill_brewer(palette = "YlGnBu", name = str_wrap(name, width = 10))+
      coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]))+
      theme_void()
  })

})
