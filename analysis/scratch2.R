dests <- 1
modes <- c(1,2,4,5)
time <- 2
agg<- 8


access <- test_access
demo <- test_demos[1]
dests <- test_dests

weights_all_for_plot <- dasy_raster %>% 
  mutate(pct_min = minority/(minority+nonminority),
         pct_nonmin = nonminority/(minority+nonminority),
         pct_min_adult = minority_adult/(minority_adult+nonminority_adult),
         pct_nonmin_adult = nonminority_adult/(minority_adult+nonminority_adult),
         pct_lowinc = lowinc/ (lowinc+ nonlowinc),
         pct_nonlowinc = nonlowinc/ (lowinc +nonlowinc),
         pct_lowinc_adult = lowinc_adult/ (lowinc_adult+ nonlowinc_adult),
         pct_nonlowinc_adult = nonlowinc_adult/ (lowinc_adult +nonlowinc_adult),
         pct_zvhh = zero_veh_hh/(zero_veh_hh+ non_zero_veh_hh)
         ) %>% 
  select(starts_with("pct"))

plot <- visualize_for_access(access, dests)
visualize_for_access_w_demo <- function(access,dests, cols = 4 ){
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
    weight <- select(dasy_raster, pop_dec)
    access_w <- access*weight
    names(access_w)<- paste("Population weighted,\n", names(access_w))
    access_weighted <- c(access, access_w)
  } else if (demo == 2){
    weight <- select(dasy_raster, pop_dec_adult)
    access_w <- access*weight
    names(access_w)<- paste("Adult population weighted,\n", names(access_w))
    access_weighted <- c(access, access_w)
  } else if (demo == 3){
    weight_ej <- select(weights_all_for_plot, pct_min_adult)
    weight_nonej <- select(weights_all_for_plot, pct_nonmin_adult)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Minorirty (Adult),\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Nonminority (Adult),\n", names(access_nonej))
    access_weighted <- c(access, access_ej, access_nonej)
  } else if (demo == 4){
    weight_ej <- select(weights_all_for_plot, pct_min)
    weight_nonej <- select(weights_all_for_plot, pct_nonmin)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Minorirty,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Nonminority,\n", names(access_nonej))
    access_weighted <- c(access, access_ej, access_nonej)
  } else if (demo == 5){
    weight_ej <- select(weights_all_for_plot, pct_lowinc_adult)
    weight_nonej <- select(weights_all_for_plot, pct_nonlowinc_adult)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Low-income (Adult),\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-low-income (Adult),\n", names(access_nonej))
    access_weighted <- c(access, access_ej, access_nonej)
  } else if (demo == 6){
    weight_ej <- select(weights_all_for_plot, pct_lowinc)
    weight_nonej <- select(weights_all_for_plot, pct_nonlowinc)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Low-income,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-low-income,\n", names(access_nonej))
    access_weighted <- c(access, access_ej, access_nonej)
  } else if (demo == 7){
    weight_ej <- select(weights_all_for_plot, pct_zvhh)
    weight_nonej <- select(weights_all_for_plot, pct_nonzvhh)
    
    access_ej <- access*weight_ej
    names(access_ej)<- paste("Zero vehicle households,\n", names(access_ej))
    access_nonej <- access*weight_nonej
    names(access_nonej)<- paste("Non-zero vehicle households,\n", names(access_nonej))
    access_weighted <- c(access, access_ej, access_nonej)
  } else (
    access_weighted <- access
  )
  
    

  
  if (length(names(access_weighted))>1 ){
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

weight <- select(dasy_raster, pop_dec) 
access_pop <- access * weight

weight <- dasy_raster %>% 
  mutate(pct_min = minority/(minority+nonminority),
         pct_nonmin = nonminority/(minority+nonminority))

weight_ej <- select(weight, pct_min)
weight_nonej <- select(weight, pct_nonmin)

access_ej <- access*weight_ej
names(access_ej)<- paste("Minorirty, ", names(access_ej))
access_nonej <- access*weight_nonej
names(access_nonej)<- paste("Nonminority", names(access_nonej))
access_weighted <- c(access, access_ej, access_nonej)

#access_d <- access_pop %>% st_redimension()
access_j <- access_weighted %>% st_redimension()

ggplot()+
  geom_stars(data = access_j)+
  geom_sf(data = commTypes_byMuni, size=.2, color = 'light gray', fill = 'transparent')+
  geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
  coord_sf()+
  scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt", #'log1p',
                      na.value = "transparent",
                      name = "Opportunities Accessible")+
  # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
  facet_wrap(~new_dim)+
  #ggtitle(paste0("Access to ", d))+
  #labs(caption= paste0("Time period: ", time_period))+
  theme_void()



ggplot()+
  geom_stars(data = weight)+
  geom_sf(data = commTypes_byMuni, size=.3, color = 'light gray', fill = 'transparent')+
  geom_sf(data=mpoBoundary,size=.4,color="light gray", fill= "transparent")+
  coord_sf()+
  scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",#trans= 'log1p',
                      na.value = "transparent",
                      name = "Opportunities Accessible")+
  # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
  #ggtitle(paste0("Access to ", d, "\n", names(access)))+
  #labs(caption= paste0("Time period: ", time_period))+
  theme_void()

ggplot()+
  geom_stars(data = access)+
  geom_sf(data = commTypes_byMuni, size=.3, color = 'light gray', fill = 'transparent')+
  geom_sf(data=mpoBoundary,size=.4,color="light gray", fill= "transparent")+
  coord_sf()+
  scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt",#trans= 'log1p',
                      na.value = "transparent",
                      name = "Opportunities Accessible")+
  # scale_fill_steps(n.breaks = 30,na.value = 'transparent')+
  #ggtitle(paste0("Access to ", d, "\n", names(access)))+
  #labs(caption= paste0("Time period: ", time_period))+
  theme_void()
