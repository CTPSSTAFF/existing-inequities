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