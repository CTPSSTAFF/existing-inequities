# number line to plot ratios for engagement material

library(tidyverse)

access_ratios <- read_csv( "app/data/access_ratios.csv") %>% 
  filter(type != "Total population") %>%
  mutate(ratio = case_when(
    region == "MPO" ~ `Ratio MPO`,
    TRUE ~ `Ratio Aggregation Area`),
    flag = ifelse(ratio <1, 1, 0)) %>% 
  select(destination, mode, region, type, ratio, flag) 

access_ratios$type <- factor(access_ratios$type)
access_ratios$flag <- factor(access_ratios$flag)
access_ratios$region <- factor(access_ratios$region, levels = c("MPO",
                                                                "Inner Core: Metro Core Communities",
                                                                "Inner Core: Streetcar Suburbs",
                                                                "Regional Urban Centers: Sub-Regional Urban Centers",
                                                                "Maturing Suburbs: Mature Suburban Towns" ,
                                                                "Maturing Suburbs: Established Suburbs and Cape Cod Towns",
                                                                "Developing Suburbs: Maturing New England Towns",
                                                                "Developing Suburbs: Country Suburbs"
                                                                ))

ggplot(access_ratios)+
  geom_rect(xmin = 0, xmax = 1, ymin = 1, ymax = 3, fill = "light gray", alpha = .5)+
  # geom_point(aes(y = type, x = ratio, color = flag), size= 10,shape = 5, alpha= .3)+
  geom_point(aes(y = type, x = ratio, color = region, shape =mode ))+
  theme_minimal()+
  ylab("Population type")+
  xlab("Ratio, (Average opportunities accessible per EJ person /\n Average opportunitites accessible per non-EJ person)")+
  facet_wrap(~destination, ncol = 1)

ggplot(access_ratios)+
  geom_rect(xmin = 0, xmax = 1, ymin = 1, ymax = 3, fill = "light gray", alpha = .5)+
  # geom_point(aes(y = type, x = ratio, color = flag), size= 10,shape = 5, alpha= .3)+
  geom_point(aes(y = type, x = ratio, color = destination, shape =mode ))+
  theme_minimal()+
  facet_wrap(~region, ncol = 1)

access_ratios_parks <- access_ratios %>% 
  filter(destination == "Open Space, all parks") %>% 
  # filter(grepl("Open Space", destination)& destination != "Open Space, paths") %>%
  filter(region != "MPO")

ggplot(access_ratios_parks)+
  geom_rect(xmin = 0, xmax = 1, ymin = 1, ymax = 3, fill = "light gray", alpha = .5)+
  # geom_point(aes(y = type, x = ratio, color = flag), size= 10,shape = 5, alpha= .3)+
  # geom_point(aes(y = type, x = ratio, color = destination, shape =mode ))+
  # geom_segment(aes(y= type,yend= type, x= 1, xend = ratio, color = mode ),size = 3, alpha =.5)+
  geom_point(aes(y= type, x= ratio, color = mode),stat='identity', size=6)  +
  geom_text(aes(label = round(ratio,2), y= type, x= ratio),color="white", size=2) +
  ylab("Population type")+
  xlab("Ratio, (Average opportunities accessible per EJ person /\n Average opportunitites accessible per non-EJ person)")+
  theme_minimal()+
  facet_wrap(~region, ncol = 1)

ggsave("output/ratio_comp_base_graphic.svg", width = 20, height = 10 )

