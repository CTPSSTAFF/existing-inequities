
# MINORITY STATUS ####

# Minority Population, ACS with Decennial Control 
# Pull minority and non-minority % for each tract from 5-yr ACS. Then apply % to decennial population
min_status_acs_dec <- function(year_acs, year_dec, state, census_geog, universe_type){
  # state <- "MA"
  # census_geog <- "tract"
  # year_acs<- 2020
  # year_dec <- 2020
if (universe_type == "total population"){
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
  
  # pop_check <- minority_status %>% 
  #   ungroup() %>% 
  #   summarize(min = sum(minority, na.rm = T),
  #             nonmin = sum(nonminority, na.rm = T)) %>% 
  #   mutate(total_pop = sum(min, nonmin))
  
  return(minority_status)} 
  else if (universe_type == "age 18 and older") {
    
    non_min_acs_adult <- paste0("B01001H_", str_pad(c(7:16, 22:31), width = 3, side = "left", pad = 0))
    total_adult <- paste0("B01001_", str_pad(c(7:25,31:49), width = 3, side = "left", pad = 0))
    
    m_acs <- append(non_min_acs_adult, total_adult)
    m_stat <- c(rep("nonmin",length(non_min_acs_adult)), 
                rep("total_adult",length(total_adult)))
    names(m_acs)<- m_stat
    
    min_acs_raw <- get_acs(geography = census_geog,
                           variables = m_acs,
                           state= state,
                           geometry = F,
                           year= year_acs) 
    min_acs <- min_acs_raw %>%
      group_by(GEOID, variable) %>% 
      summarize(est=sum(estimate),
                est_moe= moe_sum(moe,estimate)) %>%
      pivot_wider(id_cols = GEOID, names_from= variable, values_from = c(est, est_moe)) %>% 
      mutate(est_min = est_total_adult- est_nonmin,
             # caution. not sur if this is the appropriate use to the moe_sum equation. Usually used for aggregating, not subtracting
             est_min_moe = moe_sum(c(-est_moe_nonmin, est_moe_total_adult), c(-est_nonmin, est_total_adult))) %>% 
      mutate(percent_min = ifelse(est_total_adult == 0, NA, est_min/est_total_adult),
             percent_nonmin =ifelse(est_total_adult == 0, NA, est_nonmin /est_total_adult)) %>% 
      select(GEOID, percent_min_adult= percent_min, percent_nonmin_adult= percent_nonmin)
    
    dec_raw <- get_decennial(geography = census_geog,
                             variables = "P4_001N",
                             state = state,
                             geometry = FALSE,
                             year = year_dec)
    
    # Bring race acs and dec together 
    minority_status <- dec_raw %>% 
      select(-variable) %>% 
      rename(pop_dec= value) %>% 
      left_join(min_acs, by = "GEOID") %>% 
      mutate(minority_adult= pop_dec*percent_min_adult,
             nonminority_adult = pop_dec*percent_nonmin_adult)
     # adult_check <- minority_status %>%
     #   ungroup() %>% 
     #   summarize(min_adult = sum(minority_adult, na.rm=T),
     #             nonmin_adult = sum(nonminority_adult, na.rm=T)) %>% 
     #   mutate(total_adult = sum(min_adult, nonmin_adult))
    
    return(minority_status)
    
  } 
  else {
    return ("Please indicate a universe type. Options are 'total population' or 'age 18 and older'.")
  }
  
}

# Minority Population, Decennial
# Pull non-Hispanic white population from decennial, compare to total pop
min_status_dec <- function( year_dec, state, census_geog, universe_type){
if (universe_type == "total population"){
  # P2 HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE
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
  return(minstatus_dec) }
  else if (universe_type == "age 18 and older") {
    # P4 table: HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER
    v_non_min <- "P4_005N" #	!!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone
    v_total <- "P4_001N" # !!Total:
    
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
  else {return ("Please indicate a universe type. Options are 'total population' or 'age 18 and older'.")}
}

# INCOME STATUS #### 
# Low-income Threshold
get_low_inc_threshold <- function( year_acs, state, service_area, type, percent){
  # state <- "MA"
  # service_area <- boundary
  # year_acs<- 2020
  
  
  # Function finds two different types of low-income thresholds
  # If type is set to "FPL" then the function reports where to find more detail from the census on what the FPL is.
  # If type is set to "AMI" then the function reports the low-income threshold that is 60% of the Area Median Income
  # based on the service area variable using 5-year American Community Survey Household Income data for the year ending in the year_acs variable.
  if (type == "FPL"){
    #TODO: determine if we have a way to report the low income theshold using the 5year FPL ratio.
    print("Population within a ratio of the federal poverty level is calculated by the US Census. For more detail on the low-income threshold using this definition see:
          https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html")
    
  } else if (type = "AMI") {
    
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
# Low-income Population, ACS with Decennial Control 
# Pull low-income and non-low-income % for each tract from 5-yr ACS. Then apply % to decennial  population
inc_status_AMI_acs_dec <- function(year_acs, year_dec, state, low_income_threshold, census_geog){
  ##### Income distribution #####
  
  # Household income distribution by census geog
  inc_variables <- paste0("B19001_", str_pad(c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), width = 3, side = "left", pad = 0))
  inc_ranges <- c( "<10k","10k_15k","15k_20K","20k_25k","25k_30k","30k_35k","35k_40k","40k_45k",
                   "45k_50k","50k_60k","60k_75k","75k_100k","100k_125k","125k_150k","150k_200k",">200k")
  inc_low <- c(0, 10000, 15000, 20000, 25000, 30000,35000, 40000,45000, 50000, 60000, 75000, 100000, 125000, 150000, 200000)
  inc_high <- c(9999, 14999, 19999, 24999, 29999, 34999, 39999, 44999, 49999, 59999, 74999,99999, 124999, 149999, 199999, 999999)
  inc_summary_var <- "B19001_001"
  
  income_dist_raw_acs <- get_acs(geography = census_geog,
                                 variables = inc_variables,
                                 summary_var = inc_summary_var,
                                 state = state,
                                 geometry = F,
                                 cb= F,
                                 year = year_acs) 
  
  inc_dist_acs <- income_dist_raw_acs %>% 
    left_join(tibble(variable = inc_variables, inc_range = inc_ranges), by= "variable") %>% 
    left_join(tibble(inc_range= inc_ranges, inc_low= inc_low, inc_high= inc_high),by = "inc_range") %>% 
    mutate(inc_status = case_when(
      inc_high <= low_income_threshold~ "lowincome",
      inc_low > low_income_threshold ~ "nonlowincome",
      TRUE ~ "split"))
  split <- inc_dist_acs %>% 
    filter(inc_status== "split") %>% 
    mutate(estimate_low = (low_income_threshold-inc_low)/(inc_high-inc_low)*estimate,
           estimate_high= estimate- estimate_low) %>% 
    pivot_longer(cols= c(estimate_low, estimate_high)) %>%
    mutate(inc_status= ifelse(name=="estimate_low", "lowincome", "nonlowincome")) %>% 
    select(-estimate) %>% 
    rename(estimate= value)
  
  inc_status_acs <- inc_dist_acs %>% 
    filter(inc_status != "split") %>% 
    bind_rows(split) %>% 
    group_by(GEOID, inc_status) %>% 
    summarize(est=sum(estimate),
              est_moe= moe_sum(moe,estimate),
              pop_acs= first(summary_est),
              pop_acs_moe= first(summary_moe)) %>%
    group_by(GEOID, inc_status) %>% 
    summarize(percent= est/pop_acs,
              percent_moe= moe_prop(est, pop_acs, est_moe, pop_acs_moe)) %>% 
    pivot_wider(names_from= inc_status, values_from = c(percent, percent_moe))
  
  
  
  
  # get population/households from decennial
  dec_raw <- get_decennial(geography = "tract",
                           # variables = "P001001",  # Sex by Age, total.population
                           variables= "H003002", # total occupied housing units
                           state = state,
                           geometry = FALSE,
                           year = year_dec)
  
  # Bring income acs and dec together ########
  income_status <- dec_raw %>% 
    select(-variable) %>% 
    rename(hh_dec= value) %>% 
    left_join(inc_status_acs, by = "GEOID") %>% 
    mutate(lowincome= hh_dec*percent_lowincome,
           nonlowincome = hh_dec*percent_nonlowincome)
  
  return(income_status)
}

inc_status_FPL_acs_dec <- function(year_acs, year_dec, state, census_geog, universe) {
  if (universe_type == "total_population"){
    # C17002_001 Estimate!!Total: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
    # C17002_008 Estimate!!Total:!!2.00 and over RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
    inc_acs <- paste0("C17002_", str_pad(c(1:8), width = 3, side = "left", pad = 0))
    names(inc_acs)<- c("total_pop", rep("lowinc",6), "nonlowinc")

    acs_inc_raw <- get_acs(geography = census_geog,
                           variables = inc_acs,
                           state = state,
                           geometry = F,
                           year = year_acs)
    inc_acs <- acs_inc_raw %>% 
      group_by(GEOID, variable) %>% 
      summarise(est= sum(estimate),
                moe = moe_sum(moe, est)) %>%
      pivot_wider(id_cols = GEOID, names_from= variable, values_from = c(est, moe)) %>% 
      mutate(percent_lowinc = ifelse(est_total_pop == 0, NA, est_lowinc/est_total_pop),
             percent_lowinc_moe = moe_prop(est_lowinc, est_total_pop, moe_lowinc, moe_total_pop),
             percent_nonlowinc =ifelse(est_total_pop == 0, NA, est_nonlowinc /est_total_pop),
             percent_nonlowinc_moe = moe_prop(est_nonlowinc, est_total_pop, moe_nonlowinc, moe_total_pop)) %>% 
      select(GEOID, starts_with("percent_"))
      
    dec_raw <- get_decennial(geography = census_geog,
                             variables = "P1_001N",
                             state = state,
                             geometry = FALSE,
                             year = year_dec)
    # Bring race acs and dec together 
    income_status <- dec_raw %>% 
      select(-variable) %>% 
      rename(pop_dec= value) %>% 
      left_join(inc_acs, by = "GEOID") %>% 
      mutate(lowinc= pop_dec*percent_lowinc,
             nonlowinc = pop_dec*percent_nonlowinc)
    
    return(income_status)
    
  }
  else if (universe_type == "age 18 and older"){
    # B17024_001 Estimate!!Total:  AGE BY RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
    inclow_acs <- paste0("B17024_", str_pad(c(42:49,55:62,68:75,81:88,94:101,107:114,120:127), width = 3, side = "left", pad = 0))
    incnonlow_acs <- paste0("B17024_", str_pad(c(50:53,63:66,76:79,89:92,102:105,115:118, 128:131), width = 3, side = "left", pad = 0))
    total_adult_acs <- paste0("B17024_", str_pad(c(41,54,67,80,93,106,119), width = 3, side = "left", pad = 0))
    
    inc_acs <- c(inclow_acs, incnonlow_acs, total_adult_acs)
    names(inc_acs)<- c(rep("lowinc",length(inclow_acs)), rep("nonlowinc",length(incnonlow_acs)), rep("total_adult", length(total_adult_acs)))
    
    acs_inc_raw <- get_acs(geography = census_geog,
                           variables = inc_acs,
                           state = state,
                           geometry = F,
                           year = year_acs)
    
    inc_acs <- acs_inc_raw %>% 
      group_by(GEOID, variable) %>% 
      summarise(est= sum(estimate),
                moe = moe_sum(moe, est)) %>%
      pivot_wider(id_cols = GEOID, names_from= variable, values_from = c(est, moe)) %>% 
      mutate(percent_lowinc = ifelse(est_total_adult == 0, NA, est_lowinc/est_total_adult),
             percent_lowinc_moe = moe_prop(est_lowinc, est_total_adult, moe_lowinc, moe_total_adult),
             percent_nonlowinc =ifelse(est_total_adult == 0, NA, est_nonlowinc /est_total_adult),
             percent_nonlowinc_moe = moe_prop(est_nonlowinc, est_total_adult, moe_nonlowinc, moe_total_adult)) %>% 
      select(GEOID, starts_with("percent_"))
    
    dec_raw <- get_decennial(geography = census_geog,
                             variables = "P4_001N",
                             state = state,
                             geometry = FALSE,
                             year = year_dec)
    
    # Bring race acs and dec together 
    income_status <- dec_raw %>% 
      select(-variable) %>% 
      rename(pop_dec= value) %>% 
      left_join(inc_acs, by = "GEOID") %>% 
      mutate(lowinc_adult= pop_dec*percent_lowinc,
             nonlowinc_adult = pop_dec*percent_nonlowinc)
    
    return(income_status)
    
    
  }
  else {return("Please indicate a universe type. Options are 'total population' or 'age 18 and older'.")}
}

# VEHICLE ACCESS ####


