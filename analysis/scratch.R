test_data<- test_access %>% select(HealthCare_NonEmergency_AM_Drive_30min)
ej<- select(dasy_raster,zero_veh_hh)
nonej <- select(dasy_raster,non_zero_veh_hh)
pct_ej <- ej/ (ej+nonej)
pct_nonej <- 1- pct_ej
#test1 <- test_data*select(dasy_raster,minority)
test1 <- test_data*pct_ej
names(test1)<- paste0("EJ_",names(test))
#test2 <- test_data*select(dasy_raster,nonminority)
test2 <- test_data*pct_nonej
names(test2) <- paste0("NonEJ_",names(test2))

test <- c(test1,test2) %>% st_redimension()
ggplot()+
  geom_stars(data = test)+
  geom_sf(data = commTypes_byMuni, size=.2, color = 'light gray', fill = 'transparent')+
  geom_sf(data= mpoBoundary,size=.5,color='gray', fill= 'transparent')+
  coord_sf()+
  scale_fill_gradient(low= 'white', high= '#871F78' ,trans="sqrt", 
                      na.value = "transparent",
                      name = "Opportunities Accessible")+
  facet_wrap(~new_dim)+
  # ggtitle(paste0("Access to ", d))+
  # labs(caption= paste0("Time period: ", time_period))+
  theme_void()


library(biscale)
# Find zero vehicle hh by low income
pct_zeroveh<- select(dasy_raster,zero_veh_hh)/ (select(dasy_raster,zero_veh_hh)+select(dasy_raster,non_zero_veh_hh))
pct_lowinc <- select(dasy_raster, lowinc)/ (select(dasy_raster, lowinc)+select(dasy_raster, nonlowinc))

test <- pct_zeroveh %>% st_as_sf() %>% st_join(st_as_sf(pct_lowinc))
test <- test %>% bi_class(x=zero_veh_hh, y= lowinc, style= "fisher", dim =3)

map <- ggplot() +
  geom_sf(data = test, mapping = aes(fill = bi_class), color = "transparent", size = .1, show.legend = FALSE) +
  bi_scale_fill(pal = "PinkGrn", dim = 3) +
  labs(
    title = "Zero Vehicle Households and Income"
  ) +
  bi_theme()
map
legend <- bi_legend(pal = "PinkGrn",
                    dim = 3,
                    xlab = "Higher % Zero Vehicle HH",
                    ylab = "Higher % Low Income",
                    size = 8)
library(cowplot)
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.7, .45, 0.2, 0.2)
finalPlot






