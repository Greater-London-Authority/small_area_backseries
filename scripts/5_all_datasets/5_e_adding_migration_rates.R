
## 0. libraries, functions, inputs
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")


rate_max <- 0.8
c_years_to_average <- c(5, 10, 13)
age_max <- 90

base_years_for_out_rate_prior <- c(2012, 2013, 2014)

join_cols <- c("area_code", "geography", "scenario", "year", "age", "sex")
join_cols_base <- c("area_code", "geography", "scenario", "age", "sex")


## 1. read in and tidy datasets needed
estimates_parquet <- open_dataset(paste0("output_data/estimates_backseries_", max_year))

population <- estimates_parquet %>%
  filter(
    scenario == sel_scenario,
    geography == geography_name,
    component == "population"
  ) %>%
  collect() %>%
  select(-component)

births <- estimates_parquet %>%
  filter(
    scenario == sel_scenario,
    geography == geography_name,
    component == "births"
  ) %>%
  collect() %>%
  filter(age == 0) %>%
  select(-component)

outflow <- estimates_parquet %>%
  filter(
    scenario == sel_scenario,
    geography == geography_name,
    component == "out_migration"
  ) %>%
  collect() %>%
  rename(outflow = value) %>%
  select(-component)

inflow <- estimates_parquet %>%
  filter(
    scenario == sel_scenario,
    geography == geography_name,
    component == "in_migration"
  ) %>%
  collect() %>%
  rename(inflow = value) %>%
  select(-component)

year_max <- max(outflow$year)
year_min <- max(min(population$year) + 1, min(births$year), min(outflow$year))

outflow <- filter(outflow, between(year, year_min, year_max)) %>%
  split(., ~ area_code)

inflow <- filter(inflow, between(year, year_min, year_max)) %>%
  split(., ~ area_code)


## 2. get standard population at risk - standard_population_at_risk uses population and births but doesn't include inflows
standard_population_at_risk <- bind_rows(
  population %>%
    mutate(age = case_when(
      age < age_max ~ age + 1,
      TRUE ~ age_max)) %>%
    mutate(year = year + 1) %>%
    group_by(across(-any_of(c("value")))) %>%
    summarise(population_at_risk = sum(value), .groups = "drop"),
  births %>%
    rename(population_at_risk = value)
) %>%
  filter(between(year, year_min, year_max)) %>%
  split(., ~ area_code)

rm(population, births)


## 3. set up and carry out fitting of modelled flows

modelled_flows <- vector(mode = "list", length = length(inflow))
names(modelled_flows) <- names(inflow)

k <- 0

message(paste0("Fitting rates for ", length(inflow), " areas."))

for(acode in names(modelled_flows)) { # to do - fix this process, get rid of multiple nested functions, hardcoded variable names, functions without all required inputs declared, functions that need objects to have been created elsewhere in global environment, etc etc etc.......
  
  modelled_flows[[acode]] <- model_flows_single_area(standard_population_at_risk[[acode]], 
                                                     inflow[[acode]],
                                                     outflow[[acode]],
                                                     base_years = base_years_for_out_rate_prior,
                                                     max_iterations = 100)
  
  k <- k + 1
  
  if(k %% 200 == 0) message(paste0(k, " of ", length(modelled_flows)))
}

modelled_flows <- bind_rows(modelled_flows)

output_inflows <- modelled_flows %>%
  filter(component == "inflow") %>%
  rename(inflow = value) %>%
  select(-component)

output_outflows <- modelled_flows %>%
  filter(component == "outflow") %>%
  rename(outflow = value) %>%
  select(-component)

standard_population_at_risk <- bind_rows(standard_population_at_risk)

output_population_at_risk <- add_inflows_to_population_at_risk(
  standard_population_at_risk, 
  output_inflows
) 


## 4. create average rates, and save results

  ### 4.1. outflows
for(years_to_average in c_years_to_average) {
  
  out_mig_rates <- output_outflows %>%
    filter(between(year, year_min, year_max)) %>%
    left_join(output_population_at_risk, by = join_cols) %>%
    filter(between(year, year_max - (years_to_average - 1), year_max)) %>%
    group_by(across(-any_of(c("year", "outflow", "population_at_risk")))) %>%
    summarise(population_at_risk = sum(population_at_risk),
              outflow = sum(outflow),
              .groups = "drop") %>%
    mutate(population_at_risk = case_when(
      population_at_risk < outflow ~ outflow,
      population_at_risk < 1 ~ 1,
      TRUE ~ population_at_risk
    )) %>%
    mutate(value = case_when(
      outflow/population_at_risk > rate_max ~ rate_max,
      TRUE ~ outflow/population_at_risk
    )) %>%
    select(-c(population_at_risk, outflow)) %>%
    mutate(scenario = paste0(years_to_average,"_years"),
           component = "out_migration_rates",
           year = year_max + 1) %>%
    arrange(area_code, sex, age) %>%
    write_dataset(path = paste0("output_data/input_rates_", max_year),
                  format = "parquet", 
                  partitioning = c("geography", "component", "scenario", "year"))
  
}

  ### 4.2. inflows
for(years_to_average in c_years_to_average) {
  
  in_mig_flows <- output_inflows %>%
    filter(between(year, year_max - (years_to_average - 1), year_max)) %>%
    group_by(across(-any_of(c("year", "inflow")))) %>%
    summarise(value = mean(inflow), .groups = "drop") %>%
    mutate(scenario = paste0(years_to_average,"_years"),
           component = "in_migration_rates",
           year = year_max + 1) %>%
    arrange(area_code, sex, age) %>%
    write_dataset(path = paste0("output_data/input_rates_", max_year), # again rename and automate, after testing and reviewing
                  format = "parquet", 
                  partitioning = c("geography", "component", "scenario", "year"))
  
}

