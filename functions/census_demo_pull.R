# Minority Population, ACS with Decennial Control ####
# Pull minority and non-minority % for each tract from 5-yr ACS. Then apply % to decennial tract population
min_status_acs_dec <- function(year_acs, year_dec, state, census_geog){

m_acs <- paste0("B03002_", str_pad(c(3:9,12), width = 3, side = "left", pad = 0))
m_stat <- c("nonmin", rep("min",7))

min_acs_raw <- get_acs(geography = census_geog,
                       variables = m_acs,
                       summary_var = "B03002_001",
                       state= state,
                       geometry = F,
                       year= year_acs) %>% 
  left_join(tibble(variable = m_acs,
                   min_status = m_stat),by = "variable")
min_acs <- min_acs_raw %>%
  group_by(GEOID, min_status) %>% 
  summarize(est=sum(estimate),
            est_moe= moe_sum(moe,estimate),
            pop_acs= first(summary_est),
            pop_acs_moe= first(summary_moe)) %>%
  group_by(GEOID, min_status) %>% 
  summarize(percent= est/pop_acs,
            percent_moe= moe_prop(est, pop_acs, est_moe, pop_acs_moe)) %>% 
  pivot_wider(names_from= min_status, values_from = c(percent, percent_moe))
dec_raw <- get_decennial(geography = census_geog,
                         variables = "P1_001N",
                         state = state,
                         geometry = FALSE,
                         year = year_dec)

# Bring race acs and dec together 
minority_status <- dec_raw %>% 
  select(-variable) %>% 
  rename(pop_dec= value) %>% 
  left_join(min_acs, by = "GEOID") %>% 
  mutate(minority= pop_dec*percent_min,
         nonminority = pop_dec*percent_nonmin)
return(minority_status)
}

# Minority Population, Decennial ####
# Pull non-Hispanic white population from decennial, compare to total pop
min_status_dec <- function( year_dec, state, census_geog){
  
  # v20_dec <- load_variables(2020, "pl", cache= T)
  v_non_min <- "P2_005N" #!!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone
  v_total <- "P2_001N" # !!Total: # P1_001N
  
  minstatus_dec <- get_decennial(geography = census_geog,
                                       variables =v_non_min,
                                       summary_var = v_total,
                                       state = state,
                                       geometry = T,
                                       year = year_dec) %>% 
    rename(nonminority_pop = value,
           total_pop = summary_value) %>% 
    mutate(minority_pop = total_pop-nonminority_pop,
           minority_pct = minority_pop/total_pop,
           nonminority_pct = nonminority_pop/total_pop) %>% 
    select(GEOID, NAME, minority_pct, nonminority_pct, total_pop, minority_pop, nonminority_pop)
  return(minstatus_dec)
}

# Low-income Threshold ####
# TODO: FINISH OUT FUNCTION

get_low_inc_threshold <- function( year_acs, state, service_area, type){
  # state <- "MA"
  # service_area <- boundary
  # year_acs<- 2020
  
  
  # Function finds two different types of low-income thresholds
  # If type is set to "FPL" then the function reports where to find more detail from the census on what the FPL is.
  # If type is set to "AMI" then the function reports the low-income threshold that is 60% of the Area Median Income
  # based on the service area variable using 5-year American Community Survey Household Income data for the year ending in the year_acs variable.
  if (type == "FPL"){
    print("Population within a ratio of the federal poverty level is calculated by the US Census. For more detail on the low-income threshold using this definition see:
          https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html")
    
  } else if (type = "AMI") {
    percent <- .60
    
    # Household income distribution by county sub_division
    inc_variables <- paste0("B19001_", str_pad(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), width = 3, side = "left", pad = 0))
    inc_ranges <- c( "TotHH","<10k","10k_15k","15k_20K","20k_25k","25k_30k","30k_35k","35k_40k","40k_45k",
                     "45k_50k","50k_60k","60k_75k","75k_100k","100k_125k","125k_150k","150k_200k",">200k")
    
    
    income_dist_state <- get_acs(geography = "county subdivision",
                               variables = inc_variables,
                               # summary_var = inc_summary_var,
                               state = state,
                               # geometry = T,
                               year = year_acs) 
    
    # # from state, pull only towns in service area
    # inc_dist_state<- income_dist_raw %>% st_transform(26986)
    # inc_dist_service_area <-inc_dist_state[sf::st_centroid(st_transform(service_area, 26986)),]
    
    # inc_dist_state <- income_dist_raw
    inc_dist_service_area <- income_dist_raw %>% filter(GEOID %in% service_area)
    
    inc_dist <- inc_dist_service_area %>% 
      # st_drop_geometry() %>% 
      left_join(tibble(variable = inc_variables, inc_range = inc_ranges), by= "variable") %>% 
      rename(estimate_hh = estimate, 
             moe_hh = moe #,
             # est_tot_hh = summary_est,
             # moe_tot_hh = summary_moe
      ) %>% 
      select(GEOID, NAME, inc_range, estimate_hh) %>% 
      pivot_wider(names_from = inc_range, values_from = estimate_hh) %>% 
      janitor::adorn_totals() 
    
    inc<- c( "<10k","10k_15k","15k_20K","20k_25k","25k_30k","30k_35k","35k_40k","40k_45k",
             "45k_50k","50k_60k","60k_75k","75k_100k","100k_125k","125k_150k","150k_200k",">200k")
    inc_low <- c(0, 10000, 15000, 20000, 25000, 30000,35000, 40000,45000, 50000, 60000, 75000, 100000, 125000, 150000, 200000)
    inc_high <- c(9999, 14999, 19999, 24999, 29999, 34999, 39999, 44999, 49999, 59999, 74999,99999, 124999, 149999, 199999, 999999)
    
    totals <- inc_dist[inc_dist$GEOID == "Total",] %>% 
      select(-c(GEOID, NAME)) %>% 
      pivot_longer(cols= -TotHH,names_to = "inc", values_to = "hh" ) %>% 
      mutate(perc_hh_sa= hh/TotHH*100) %>% 
      mutate(cumsum = cumsum(perc_hh_sa)) %>% 
      left_join(tibble(inc= inc, inc_low=inc_low, inc_high= inc_high), by="inc") %>% 
      mutate(med_calc = ifelse(cumsum >= 50 & lag(cumsum <50),
                               round(inc_low+(inc_high-inc_low)*(50-lag(cumsum))/(cumsum-lag(cumsum)),2),
                               0))
    
    median_income_service_area <-totals$med_calc[which(totals$med_calc!=0)]
    low_inc_threshold <- round(median_income_service_area*percent,2)
    return(low_inc_threshold)
    
  } else {
    print("Please specify the type of Low-income threshold to calculate. Options are 1) `type = 'FPL'` to find the low-income threshold that is 200% of the Federal Poverty Level
    or 2) `type = 'AMI'` to find the low-income threshold that is 60% of the area median income.")
  }
  return(NA)
}

# Low-income Population, ACS with Decennial Control ####
# Pull minority and non-minority % for each tract from 5-yr ACS. Then apply % to decennial tract population
# TODO: Finish out function
inc_status_acs_dec <- function(year_acs, year_dec, state, low_income_threshold, census_geog) {
  
}