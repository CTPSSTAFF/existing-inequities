#
library(shiny)
library(tidyverse)
library(sf)
library(ragg)
library(ggiraph)
library(stars)
library(reactable)

shiny.useragg = TRUE

mpoBoundary <- st_read("data/AggregationAreas.gpkg", "MPO_Boundary") %>%
  st_transform(3857)
commTypes_byMuni <- st_read("data/AggregationAreas.gpkg", "CommunityTypes") %>% st_transform(3857)
# commTypes<- commTypes_byMuni %>% 
#   group_by(communityType) %>% 
#   summarize(geometry = st_union(geom))



# higherEd <- st_read("data/DestinationData.gpkg", "higherEd_PT")
# healthcare <- st_read("data/DestinationData.gpkg", "healthcare_PT")
# # TODO: spell essential correctly and re-save destination data.
# essentialPlace_poly <- st_read("data/DestinationData.gpkg", "essentailPlace_Final_POLY")
# essentialPlace_pt <- st_read("data/DestinationData.gpkg", "essentailPlace_Final_PT")

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
visualize_for_access <- function(access,dests, cols = 4 ){
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
  if (time_period ==  "AM Peak"){
    m <- word(word(names(access), sep = "_..min"), sep = "_.M_", start = 2)
  } else if(time_period ==  "Midday") {
    m <- word(word(names(access), sep = "_..min"), sep = "_Midday_", start = 2)
  } 
  else{
    m <- word(word(names(access), sep = "_..min"), sep = "_Weekend_", start = 2)
  }
  n <- paste(t, m)
  names(access)<- n
  
  if (length(names(access))>1 ){
    access <- access %>% st_redimension()
    ggplot()+
      geom_stars(data = access)+
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
  } else {
    ggplot()+
      geom_stars(data = access)+
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

get_weighted_avgs <- function(access_layer, weights){
  # access_layer <- highered$HigherEd_MD_TransitAll_30min
  access_vector <- as.vector(access_layer)
  weighted_mean_for_avg <- function(w){result <- weighted.mean(access_vector, as.vector(w), na.rm=T)}
  weighted_avgs <- lapply(weights, weighted_mean_for_avg) %>% 
    enframe() %>%
    mutate(
      pop = name,
      AvgAccessOpps = as.numeric(value)) %>% 
    rowwise() %>% 
    mutate(
      type = str_split(pop, "_")[[1]][1],
      type = case_when(
        grepl("minority_adult", pop) ~ "Minority Status, adult",
        grepl("min", pop) ~ "Minority Status",
        grepl("inc_adult", pop) ~ "Income status, adult",
        grepl("inc", pop) ~ "Income status",
        grepl("veh_hh", pop) ~ "Household vehicles",
        grepl("pop_dec_adult", pop)~ "Total population, adult",
        grepl("pop_dec", pop) ~ "Total population",
        grepl("hh_dec", pop) ~ "Total households"),
      pop_name= case_when(
        pop == "minority_adult" ~ "Minority, adult",
        pop == "nonminority_adult" ~ "Nonminority, adult",
        pop == "minority" ~ "Minoirty",
        pop == "nonminority" ~ "Nonminority",
        pop == "lowinc" ~ "Low-income",
        pop == "nonlowinc" ~ "Non-low-income",
        pop == "lowinc_adult" ~ "Low-income, adult",
        pop == "nonlowinc_adult" ~ "Non-low-income, adult",
        pop == "zero_veh_hh" ~ "Zero vehicle households",
        pop == "non_zero_veh_hh" ~ "Non-zero vehicle households",
        grepl("pop_dec_adult", pop)~ "Total population, adult",
        grepl("pop_dec", pop) ~ "Total population",
        grepl("hh_dec", pop) ~ "Total households"))
  # TODO: develop ratio and percent outputs for comparisons
  
  
  return(weighted_avgs)
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
    # dests <- 1 
    # modes <- c(1,2,3,4)
    # aggA <- 8
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
    # test_access_mpo <<- access
    if (agg == 1) { access <- access * (comm_types_rast %>% select(id1))}
    if (agg == 2) { access <- access * (comm_types_rast %>% select(id2))}
    if (agg == 3) { access <- access * (comm_types_rast %>% select(id3))}
    if (agg == 4) { access <- access * (comm_types_rast %>% select(id4))}
    if (agg == 5) { access <- access * (comm_types_rast %>% select(id5))}
    if (agg == 6) { access <- access * (comm_types_rast %>% select(id6))}
    if (agg == 7) { access <- access * (comm_types_rast %>% select(id7))}
    # if (agg == 8) { access <- access * (comm_types_rast %>% select(id))}
    
    access_rv$access <- access
    test_access <<- access
    plot <- visualize_for_access(access,dests, cols= length(time))
    plot
    
  })
  
  output$avgs<- renderReactable({
    
    access_all <- access_rv$access_mpo
    access_avgs_mpo <- tibble()
    avgs <- lapply(access_all, get_weighted_avgs,weights= dasy_raster)
    avgs <- plyr::ldply(avgs, data.frame) %>% 
      mutate(region = "MPO")
    access_avgs_mpo <- bind_rows(access_avgs_mpo,
                                   avgs)
    access_avgs_tbl <- access_avgs_mpo %>%
      select(.id, region, type, pop_name, AvgAccessOpps) %>% 
      mutate(type_detail = case_when(
        grepl('Total', type)~ "Total",
        grepl("Non", pop_name) ~ "Non-EJ",
        TRUE ~ "EJ"),
        type2 = ifelse(grepl("adult", pop_name), "Adult", NA)) %>% 
      pivot_wider(id_cols = c(.id,region, type, type2), names_from = type_detail, values_from = c(AvgAccessOpps) ) %>% 
      rowwise() %>% 
      mutate(Ratio = round(EJ/`Non-EJ`, 3)) %>% 
      mutate(time=word(.id, start = -1, sep = "_"),
             mode= word(.id, start= -2, end= -2, sep="_")) %>% 
      select(mode, time, everything()) %>% 
      select(-c(.id, type2)) %>% 
      mutate_if(is.numeric, round, digits=3)
    
    agg <- input$aggArea
    if (agg == 1) { comm_filter <- comm_types_rast %>% select(id1)}
    if (agg == 2) { comm_filter <- comm_types_rast %>% select(id2)}
    if (agg == 3) { comm_filter <- comm_types_rast %>% select(id3)}
    if (agg == 4) { comm_filter <- comm_types_rast %>% select(id4)}
    if (agg == 5) { comm_filter <- comm_types_rast %>% select(id5)}
    if (agg == 6) { comm_filter <- comm_types_rast %>% select(id6)}
    if (agg == 7) { comm_filter <- comm_types_rast %>% select(id7)}
    if (agg == 8) { comm_filter <- comm_types_rast %>% select(id)}
    dasy_filtered <- dasy_raster * comm_filter
    
    access_all_agg <- access_rv$access
    access_avgs_agg <- tibble()
    
    avgs <- lapply(access_all_agg, get_weighted_avgs,weights= dasy_filtered)
    avgs <- plyr::ldply(avgs, data.frame) %>% 
      mutate(agg_id = as.numeric(agg)) %>%
      left_join(comm_types_id, by= c("agg_id" = "id")) %>% 
      mutate(region = paste0(communityType, ': ', subtype)) %>% 
      select(-c(agg_id, communityType, subtype))
    access_avgs_agg <- bind_rows(access_avgs_agg,
                             avgs)
    
    access_avgs_tbl_agg <- access_avgs_agg %>%
      select(.id, region, type, pop_name, AvgAccessOpps) %>% 
      mutate(type_detail = case_when(
        grepl('Total', type)~ "Total",
        grepl("Non", pop_name) ~ "Non-EJ",
        TRUE ~ "EJ"),
        type2 = ifelse(grepl("adult", pop_name), "Adult", NA)) %>% 
      pivot_wider(id_cols = c(.id,region, type, type2), names_from = type_detail, values_from = c(AvgAccessOpps) ) %>% 
      rowwise() %>% 
      mutate(Ratio = round(EJ/`Non-EJ`, 3)) %>% 
      mutate(time=word(.id, start = -1, sep = "_"),
             mode= word(.id, start= -2, end= -2, sep="_")) %>% 
      select(mode, time, everything()) %>% 
      select(-c(.id, type2)) %>% 
      mutate_if(is.numeric, round, digits=3)
    access_avgs_tbl<- access_avgs_tbl %>% 
      left_join(access_avgs_tbl_agg, 
                by = c("mode", "time", "type"), 
                suffix = c("_MPO","_Agg")) %>% 
      select(-starts_with("region"))
    
    reactable(access_avgs_tbl, compact = TRUE)
  })
  
  output$avgs_agg <- renderReactable({
    agg <- input$aggArea
    if (agg == 1) { comm_filter <- comm_types_rast %>% select(id1)}
    if (agg == 2) { comm_filter <- comm_types_rast %>% select(id2)}
    if (agg == 3) { comm_filter <- comm_types_rast %>% select(id3)}
    if (agg == 4) { comm_filter <- comm_types_rast %>% select(id4)}
    if (agg == 5) { comm_filter <- comm_types_rast %>% select(id5)}
    if (agg == 6) { comm_filter <- comm_types_rast %>% select(id6)}
    if (agg == 7) { comm_filter <- comm_types_rast %>% select(id7)}
    if (agg == 8) { comm_filter <- comm_types_rast %>% select(id)}
    dasy_filtered <- dasy_raster * comm_filter
    
    access_all <- access_rv$access
    access_avgs <- tibble()
    
    avgs <- lapply(access_all, get_weighted_avgs,weights= dasy_filtered)
    avgs <- plyr::ldply(avgs, data.frame) %>% 
      mutate(agg_id = as.numeric(agg)) %>%
      left_join(comm_types_id, by= c("agg_id" = "id")) %>% 
      mutate(region = paste0(communityType, ': ', subtype)) %>% 
      select(-c(agg_id, communityType, subtype))
    access_avgs <- bind_rows(access_avgs,
                             avgs)
    
    access_avgs_tbl <- access_avgs %>%
      select(.id, region, type, pop_name, AvgAccessOpps) %>% 
      mutate(type_detail = case_when(
        grepl('Total', type)~ "Total",
        grepl("Non", pop_name) ~ "Non-EJ",
        TRUE ~ "EJ"),
        type2 = ifelse(grepl("adult", pop_name), "Adult", NA)) %>% 
      pivot_wider(id_cols = c(.id,region, type, type2), names_from = type_detail, values_from = c(AvgAccessOpps) ) %>%
      rowwise() %>% 
      mutate(Ratio = round(EJ/`Non-EJ`, 3)) %>% 
      mutate(time=word(.id, start = -1, sep = "_"),
             mode= word(.id, start= -2, end= -2, sep="_")) %>% 
      select(mode, time, everything()) %>% 
      select(-c(.id, type2)) %>% 
      mutate_if(is.numeric, round, digits=3)
    #elect(Run = .id,type, type2, pop_ region)
    
    reactable(access_avgs_tbl, compact = TRUE)
  })
  
  # ranges2 <- reactiveValues(x = NULL, y = NULL)
    output$map <- renderGirafe({

        overview_map <- ggplot()+
          geom_sf(data = mpoBoundary, color = "black", size = 1.4, fill = "white")+
          geom_sf_interactive(data = commTypes, aes(fill = communityType, 
                                                    tooltip= communityType))+
          theme_void()+ 
        theme(legend.position="none")
        girafe(ggobj = overview_map)

    })
    output$mapZoom <- renderGirafe({
      detail_map <- ggplot()+
        geom_sf(data = mpoBoundary, color = "black", size = 1.4, fill = "white")+
        geom_sf_interactive(data = commTypes, aes(fill = communityType, tooltip = communityType))+
        geom_sf(data = higherEd, aes(size = enrollment), color= 'light gray', fill = 'transparent', alpha = .6)+
        geom_sf(data = essentialPlace_poly, fill= "white", alpha =.8)+
        geom_sf(data = healthcare, aes(shape = type), size= 2, color= 'brown', alpha= .7)+
        # coord_sf(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+
        theme_void()+
        #theme(legend.title = element_text( size=10), legend.text=element_text(size=10))
        theme(legend.position="none")
        
      detail_map <- girafe(ggobj= detail_map)
      # detail_map <- girafe_options(detail_map,opts_zoom(min = .7, max = 10))
      detail_map
    })
    # observe({
    #   brush <- input$map_zoom
    #   if (!is.null(brush)) {
    #     ranges2$x <- c(brush$xmin, brush$xmax)
    #     ranges2$y <- c(brush$ymin, brush$ymax)
    #     
    #   } else {
    #     ranges2$x <- NULL
    #     ranges2$y <- NULL
    #   }
    # })

})
