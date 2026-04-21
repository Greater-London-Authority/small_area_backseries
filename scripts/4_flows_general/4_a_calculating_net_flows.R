## calculating netflows using residual differencing method
## taking population and components (births and deaths), fitting them to the same geography, differencing them and taking net flows to be the remainder

## 0. libraries and functions 
library(data.table)
library(gsscoder)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")


## 1. reading in the data

  ### 1.1. population data
population <- readRDS(paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_lsoa21.rds"))

deaths <- readRDS("input_data/intermediate/fitted_lsoa11_deaths.rds")

deaths_24_onward <- readRDS(paste0("input_data/intermediate/fitted_lsoa21_deaths", "_to_", max_year, ".rds"))

births <- readRDS(paste0("input_data/intermediate/births_2001_", max_year, "_oa21.rds"))

  ### 1.2. lookups
lsoa11_lookup_weighted <- readRDS(lsoa11_lookup_path)

if(grepl(pattern = "rds", x = oa21_lookup_path)){ 
  
  oa21_lookup <- data.table(readRDS(oa21_lookup_path))
  
}else if(grepl(pattern = "csv", x = oa21_lookup_path)){
  
  oa21_lookup <- fread(oa21_lookup_path)
  
} # hmm, this seems a bit horrible...maybe I should write a function to check for rds or csv and read in as data.table in either case


if(grepl(pattern = "rds", x = lsoa21_lookup_path)){
  
  lsoa21_lookup <- data.table(readRDS(lsoa21_lookup_path))
  
}else if(grepl(pattern = "csv", x = lsoa21_lookup_path)){
  
  lsoa21_lookup <- fread(lsoa21_lookup_path)
  
}


oa21_lookup_extended <- extend_lookup_and_add_weight(oa21_lookup, year_start = 2011, year_end = 2024, add_weight = log_oa21_lookup_best_fit) # for best fit lookups, converting them into "weighted" lookups by year, with all weights equal to 1. If it's already weighted, extend the lookups to the years we need and keeps the original weight
lsoa21_lookup_extended <- extend_lookup_and_add_weight(lsoa21_lookup, year_start = 2011, year_end = 2024, add_weight = log_lsoa21_lookup_best_fit) 


## 2. aggregating all input datasets to common age categories and common geographies, and calculating cohort

  ### 2.1. population
population <- population[year <= max_year, -"lsoa21nm"]

population <- aggregate_geographies_weighted(data = population, lookup = lsoa21_lookup_extended,
                                             geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd",
                                             geog_to_lookup = dest_geog_colname, count_names = "population")

population[ , cohort := year - age]

  ### 2.2. deaths
deaths <- aggregate_geographies_weighted(data = deaths[year >= min_year, ], lookup = lsoa11_lookup_weighted,
                                         geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd",
                                         geog_to_lookup = dest_geog_colname, count_name = "deaths")

deaths[, cohort := year - age]

deaths_24_onward <- aggregate_geographies_weighted(data = deaths_24_onward, lookup = lsoa21_lookup_extended,
                                                   geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd", 
                                                   geog_to_lookup = dest_geog_colname, count_names = "deaths")

deaths_24_onward[, cohort := year - age]

if(max_year >= 2024){
  
  deaths <- rbind(deaths, 
                 deaths_24_onward)
  
}

  ### 2.3. births
births <- births[year >= min_year & year <= max_year, ]

births <- aggregate_geographies_weighted(data = births, lookup = oa21_lookup_extended, 
                                         geog_from_data = "oa21cd", geog_from_lookup = "oa21cd",
                                         geog_to_lookup = dest_geog_colname, count_names = "births")


## 3. getting net flows for the middle cohorts
population_prev <- copy(population)

population_prev[, year := year + 1]

population_prev <- population_prev[, -"age"]
colnames(population_prev)[colnames(population_prev) == "population"] <- "population_last_year"

join_colnames <- c(dest_geog_colname, "year", "sex", "cohort")

setkeyv(population, join_colnames)
setkeyv(population_prev, join_colnames)

flows <- population_prev[population]

flows <- flows[!is.na(population_last_year), ]
flows[!is.na(population_last_year), gross_flows := population - population_last_year]

join_colnames <- c(dest_geog_colname, "year", "sex", "age", "cohort")

setkeyv(deaths, join_colnames)
setkeyv(flows, join_colnames)

flows <- deaths[flows]

flows[, gross_flows := gross_flows + deaths]


## 4. getting net flows for the starting cohort
births[, age := 0]
births[, cohort := year]

population_0 <- population[age == 0, ]

join_colnames <- c(dest_geog_colname, "year", "age", "sex", "cohort")

setkeyv(population_0, join_colnames)
setkeyv(births, join_colnames)

flows_0 <- births[population_0]

flows_0[is.na(births), births := 0]

join_colnames <- c(dest_geog_colname, "year", "age", "sex", "cohort")

setkeyv(flows_0, join_colnames)
setkeyv(deaths, join_colnames)

flows_0 <- deaths[flows_0]

flows_0[, gross_flows := population - births + deaths]


flows[, births := NA]
flows_0[, population_last_year := NA]

flows <- rbind(flows_0, flows)


## 5. getting flows for the end cohort

  ### 5.1. preparing the dataset containing the 90+ cohort in the previous year
population_end <- population[age %in% 89:90, ]

population_end_prev <- data.table(copy(population_end))
population_end_prev <- population_end_prev[ ,-"cohort"]

population_end_prev <- data.table::dcast(population_end_prev, 
                                         ... ~ age, ## this line means all other variables not already named (... symbol) is pivoted wide on age. We've specified population as the value variable below, so that one isn't included. 
                                         value.var = "population")

colnames(population_end_prev)[colnames(population_end_prev) == 89] <- "population_prev_89"
colnames(population_end_prev)[colnames(population_end_prev) == 90] <- "population_prev_90"

population_end_prev[, year := year + 1]

  ### 5.2. joining the two datasets (this year and last year)
population_end <- population_end[age == 90, ]

join_colnames <- c(dest_geog_colname, "year", "sex")
setkeyv(population_end, join_colnames)
setkeyv(population_end_prev, join_colnames)

flows_end <- population_end_prev[population_end]
flows_end <- flows_end[year %in% min_year:max_year, ]

  ### 5.3. joining deaths and calculating flows
join_colnames <- c(dest_geog_colname, "year", "sex", "age", "cohort")
setkeyv(flows_end, join_colnames)
setkeyv(deaths, join_colnames)

flows_end <- deaths[flows_end]

flows_end[, gross_flows := population - population_prev_90 - population_prev_89 + deaths]

  ### 5.4. adding the end cohort on to the rest of the flows
flows_end[, population_last_year := population_prev_90 + population_prev_89]
flows_end[, births := NA]

flows_end <- flows_end[, -c("population_prev_90", "population_prev_89")]


col_ords <- c(dest_geog_colname, "year", "age", "sex", "deaths", "cohort", "births", "population", "gross_flows", "population_last_year")
flows_end <- flows_end[, ..col_ords]

flows <- flows[age != 90, ]

flows_fin <- rbind(flows, flows_end)


## 6. saving the dataset
flows_fin <- flows_fin[year >= min_year + 1, ]

saveRDS(object = flows_fin,
        file = paste0("input_data/intermediate/", "net_flows_", dest_geog_colname, "_", min_year + 1, "_", max_year, ".rds"))

rm(list = ls())
gc()
