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
ggplot(access_ratios)+
  geom_rect(xmin = 0, xmax = 1, ymin = 1, ymax = 3, fill = "light gray", alpha = .5)+
  # geom_point(aes(y = type, x = ratio, color = flag), size= 10,shape = 5, alpha= .3)+
  geom_point(aes(y = type, x = ratio, color = region, shape =mode ))+
  theme_minimal()+
  facet_wrap(~destination, ncol = 1)

ggsave("output/ratio_comp_base_graphic.svg", width = 20, height = 10 )
