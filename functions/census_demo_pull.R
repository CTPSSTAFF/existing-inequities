# Minority Population, ACS with Decennial Control ####
# Pull minority and non-minority % for each tract from 5-yr ACS. Then apply % to decennial tract population
min_status_by_tract_acs_dec <- function(year_acs, year_dec, state){

m_acs <- paste0("B03002_", str_pad(c(3:9,12), width = 3, side = "left", pad = 0))
m_stat <- c("nonmin", rep("min",7))

min_acs_raw <- get_acs(geography = "tract",
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
dec_raw <- get_decennial(geography = "tract",
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
min_status_by_tract_dec <- function( year_dec, state){
  
  # v20_dec <- load_variables(2020, "pl", cache= T)
  v_non_min <- "P2_005N" #!!Total:!!Not Hispanic or Latino:!!Population of one race:!!White alone
  v_total <- "P2_001N" # !!Total: # P1_001N
  
  minstatus_tract_dec <- get_decennial(geography = "tract",
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
  return(minstatus_tract_dec)
}

# Low-income Threshold ####
# TODO: FINISH OUT FUNCTION
find_low_inc_threshold <- function( year_acs, state, service_area, type){
  # Function finds two different types of low-income thresholds
  # If type is set to "FPL" then the function reports the low-income threshold that is 
  # 200% of the federal poverty level for the year specified in the year_acs variable.
  # If type is set to "AMI" then the function reports the low-income threshold that is 60% of the Area Median Income
  # based on the service area variable using 5-year American Community Survey Household Income data for the year ending in the year_acs variable.
  if (type == "FPL"){
    
  } else if (type = "AMI") {
    
  } else {
    print("Please specify the type of Low-income threshold to calculate. Options are 1) `type = 'FPL'` to find the low-income threshold that is 200% of the Federal Poverty Level
    or 2) `type = 'AMI'` to find the low-income threshold that is 60% of the area median income.")
  }
  
}

# Low-income Population, ACS with Decennial Control ####
# Pull minority and non-minority % for each tract from 5-yr ACS. Then apply % to decennial tract population
# TODO: Finish out function
inc_status_by_tract_acs_dec <- function(year_acs, year_dec, state, low_income_threshold) {
  
}