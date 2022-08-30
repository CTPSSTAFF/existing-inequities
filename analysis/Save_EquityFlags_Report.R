# Summarize Equity Flags for Report writing

library(tidyverse)

access_ratios <- read_csv( "app/data/access_ratios.csv") %>% 
  filter(type != "Total population") %>%
  mutate(ratio = case_when(
    region == "MPO" ~ `Ratio MPO`,
    TRUE ~ `Ratio Aggregation Area`),
    flag = ifelse(ratio <1, 1, 0)) %>% 
  select(destination, mode, region, type, ratio, flag) 

flags_by_region <- access_ratios %>% 
  group_by(destination, region, type) %>% 
  summarize(n_flags = sum(flag)) %>% 
  pivot_wider(id_cols = c(destination, region), values_from = n_flags, names_from= type) %>% 
  arrange(destination, region)

flags_by_pop_mode <- access_ratios %>% 
  group_by(type, mode) %>% 
  summarise(n_flags= sum(flag),
            total_potential_flags = n()) %>% 
  pivot_wider()

flags_by_destination <- access_ratios %>% 
  group_by(destination) %>% 
  summarise(n_flags = sum(flag),
            total_potential_flags = n()) %>% 
  mutate(pct_flag = round(n_flags/total_potential_flags, 2))


flags_by_mode <- access_ratios %>% 
  group_by(mode) %>% 
  summarise(n_flags = sum(flag),
            total_potential_flags = n()) %>% 
  mutate(pct_flag = round(n_flags/total_potential_flags, 2))

flags_by_region_pop <- access_ratios %>% 
  group_by(region, type) %>% 
  summarise(n_flags = sum(flag),
            total_potential_flags = n()) %>% 
  mutate(pct_flag = round(n_flags/total_potential_flags, 2))

flags_by_region_mode <- access_ratios %>% 
  group_by(region, mode) %>% 
  summarise(n_flags = sum(flag),
            total_potential_flags = n()) %>% 
  mutate(pct_flag = round(n_flags/total_potential_flags, 2))

library(clipr)
write_clip(access_ratios)
write_clip(flags_by_region)
write_clip(flags_by_pop_mode)
write_clip(flags_by_destination)
write_clip(flags_by_mode)
write_clip(flags_by_region_pop)
write_clip(flags_by_region_mode)
