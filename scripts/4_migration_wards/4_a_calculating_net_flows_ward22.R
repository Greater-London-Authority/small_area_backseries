
## 0. libraries and functions 
library(data.table)
library(gsscoder)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")

paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_ward22.rds") # min and max year will work here, but not set up so that it's general..TO DO, fix this
## 1. reading in the data

  ### 1.1. population data
population <- readRDS(paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_ward22.rds"))

deaths <- readRDS("input_data/intermediate/fitted_lsoa11_deaths.rds")

deaths_24 <- readRDS("input_data/intermediate/fitted_lsoa21_deaths.rds")

births <- readRDS("input_data/intermediate/births_2001_2024_oa21.rds")

  ### 1.2. lookups (TO DO - this bit has to be made general wrt geography. I changed it manually to wards)
lsoa11_ward22_weighted <- readRDS("lookups/lsoa11_ward22_weighted_extended.rds")

lsoa21_ward22 <- readRDS("lookups/lsoa21_ward22_bf.rds")

oa21_ward22 <- readRDS("lookups/oa21_ward22_bf.rds")


## 2. aggregating all geographies to ward22cd and to common age categories, and calculating cohort

  ### 2.1. population (already at ward)
population <- population[year <= max_year, -"ward22nm"]

population <- aggregate_to_lower_max_age(input_data = population, new_max_age = 85, count_names = "population")

population[ , cohort := year - age]

  ### 2.2. deaths
deaths <- aggregate_geographies_weighted(data = deaths[year >= min_year, ], lookup = lsoa11_ward22_weighted,
                                         geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd",
                                         geog_to_lookup = "ward22cd", count_name = "deaths")

deaths[, cohort := year - age]
deaths <- deaths[, -c("lad23cd")]

deaths_24 <- aggregate_geographies_2(data = deaths_24, lookup = lsoa21_ward22,
                                     geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd", geog_to_lookup = "ward22cd",
                                     count_names = "deaths")
deaths_24 <- deaths_24[, -"lad23cd"]
deaths_24[, cohort := year - age]

if(max_year >= 2024){
  
  deaths <- rbind(deaths, 
                 deaths_24)
  
}

  ### 2.3. births
births <- births[year >= min_year & year <= max_year, ]

births <- aggregate_geographies_2(data = births, lookup = oa21_ward22, # NOTE - be aware that I updated the process for preparing the births data. Quite sure that the data is correct and in the same format, but in case something goes wrong check here. 
                                  geog_from_data = "oa21cd", geog_from_lookup = "oa21cd",
                                  geog_to_lookup = "ward22cd", count_names = "births")

  ### 2.4. should have a separate section here I feel for filtering by year, rather than doing it in the section above in a slightly scattered way. A bit neater/clearer. 
  ### also, overall, the script to this exact point here is what I need to convert to general functions with respect to years and geography to make the whole thing flexible. Also need to change geography labels below, but that's a lot easier.

## 3. the middle cohorts
population_prev <- copy(population)

population_prev[, year := year + 1]

population_prev <- population_prev[, -"age"]
colnames(population_prev)[colnames(population_prev) == "population"] <- "population_last_year"

setkey(population,"ward22cd","year", "sex", "cohort")
setkey(population_prev, "ward22cd","year", "sex", "cohort")

flows <- population_prev[population]

flows <- flows[!is.na(population_last_year), ]
flows[!is.na(population_last_year), gross_flows := population - population_last_year]


setkey(deaths, "ward22cd", "year", "sex", "age", "cohort")
setkey(flows, "ward22cd", "year", "sex", "age", "cohort")

flows <- deaths[flows]

flows[, gross_flows := gross_flows + deaths]


## 4. the starting cohort
births[, age := 0]
births[, cohort := year]

population_0 <- population[age == 0, ]

setkey(population_0, "ward22cd", "year", "age", "sex", "cohort")
setkey(births, "ward22cd", "year", "age", "sex", "cohort")

flows_0 <- births[population_0]

flows_0[is.na(births), births := 0]

setkey(flows_0, "ward22cd", "year", "sex", "age", "cohort")
setkey(deaths, "ward22cd", "year", "sex", "age", "cohort")

flows_0 <- deaths[flows_0]

flows_0[, gross_flows := population - births + deaths]


flows[, births := NA]
flows_0[, population_last_year := NA]

flows <- rbind(flows_0, flows)


## 5. the end cohort
population_end <- population[age %in% 84:85, ]

population_end_prev <- data.table(copy(population_end))

population_end_prev <- data.table::dcast(population_end_prev[ ,-"cohort"], ward22cd + year + sex ~ age, value.var = "population") 

colnames(population_end_prev)[4:5] <- c("population_prev_84", "population_prev_85")

population_end_prev[, year := year + 1]

  ### 5.2. joining the two datasets
population_end <- population_end[age == 85, ]

setkey(population_end, "ward22cd", "year", "sex")
setkey(population_end_prev, "ward22cd", "year", "sex")

flows_end <- population_end_prev[population_end]
flows_end <- flows_end[year %in% min_year:max_year, ]

  ### 5.3. joining deaths and calculating flows
setkey(flows_end, "ward22cd", "year", "sex", "age", "cohort")

flows_end <- deaths[flows_end]

flows_end[, gross_flows := population - population_prev_85 - population_prev_84 + deaths]

  ### 5.4. adding the end cohort on to the rest of the flows
flows_end[, population_last_year := population_prev_85 + population_prev_84]
flows_end[, births := NA]

flows_end <- flows_end[, -c("population_prev_85", "population_prev_84")]

flows_end <- flows_end[, c("ward22cd", "year", "age", "sex", "deaths", "cohort", "births", "population", "gross_flows", "population_last_year")]

flows <- flows[age != 85, ]

flows_fin <- rbind(flows, flows_end)


## 6. saving the dataset
flows_fin <- flows_fin[year >= 2012, ] # only have population at lsoa21 back to 2011, so of course can't get estimate for anything earlier than 2012. So fine to have this just coded in. 

saveRDS(object = flows_fin,
        file = "input_data/intermediate/net_flows_ward22cd.rds")

rm(list = ls())
gc()
