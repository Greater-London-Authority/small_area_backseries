
## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")


## 1. reading in the data

  ### 1.1. population data
flows <- readRDS(paste0("input_data/intermediate/estimated_gross_flows_", min_year, "_", max_year, ".rds"))

population <- readRDS(paste0("input_data/intermediate/mid_year_rebased_2011", max_year, "_lsoa21.rds"))

deaths <- readRDS("input_data/intermediate/fitted_lsoa11_deaths.rds")

deaths_24 <- readRDS("input_data/intermediate/fitted_lsoa21_deaths.rds")

births <- readRDS("input_data/intermediate/births_2001_2024_oa21.rds")


### 1.2. lookups
oa11_msoa21_weighted <- readRDS("lookups/oa11_msoa21_weighted_extended.rds")

lsoa11_msoa21_weighted <- readRDS("lookups/lsoa11_msoa21_weighted_extended.rds")

lsoa21_msoa21 <- fread("lookups/2021_oa_lsoa_msoa_la.csv")

oa21_msoa21 <- unique(lsoa21_msoa21[, c("oa21cd", "msoa21cd")])
lsoa21_msoa21 <- unique(lsoa21_msoa21[, c("lsoa21cd", "msoa21cd")])

msoa21_lad23 <- fread("lookups/msoa21_lad23.csv")

colnames(msoa21_lad23) <- tolower(colnames(msoa21_lad23))


## 2. aggregating all geographies to msoa21cd and to common age categories, and calculating cohort

### 2.1. population
population <- population[year <= max_year, -"lsoa21nm"]

population <- aggregate_geographies_2(data = population, lookup = lsoa21_msoa21,
                                      geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd", geog_to_lookup = "msoa21cd",
                                      count_names = "population")

population <- aggregate_to_lower_max_age(input_data = population, new_max_age = 75, count_names = "population")

population[ , cohort := year - age]
population <- population[, -c("lad21cd", "lad21nm")] # just easier to get rid of these


### 2.2. deaths
deaths <- aggregate_geographies_weighted(data = deaths[year >= min_year, ], lookup = lsoa11_msoa21_weighted,
                                         geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd",
                                         geog_to_lookup = "msoa21cd", count_name = "deaths")

deaths <- deaths[, -c("lad23cd")]

deaths_24 <- aggregate_geographies_2(data = deaths_24, lookup = lsoa21_msoa21,
                                     geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd", geog_to_lookup = "msoa21cd",
                                     count_names = "deaths")
deaths_24 <- deaths_24[, -"lad23cd"]

if(max_year >= 2024){
  
  deaths <- rbind(deaths, 
                  deaths_24)
  
}

deaths <- aggregate_to_lower_max_age(deaths, new_max_age = 75, count_names = "deaths") # TO DO - make a note in this function. Can't be both age and cohort in the dataset - must be just age. 

deaths[, cohort := year - age]

### 2.3. births
births <- births[year >= min_year & year <= max_year, ]

births <- aggregate_geographies_2(data = births, lookup = oa21_msoa21, # NOTE - be aware that I updated the process for preparing the births data. Quite sure that the data is correct and in the same format, but in case something goes wrong check here. 
                                  geog_from_data = "oa21cd", geog_from_lookup = "oa21cd",
                                  geog_to_lookup = "msoa21cd", count_names = "births")

births[, age := 0]

## 4. joining the datasets, adding on la codes
setkey(population, "msoa21cd", "year", "age", "sex")
setkey(births, "msoa21cd", "year", "age", "sex")
setkey(deaths, "msoa21cd", "year", "age", "sex")
setkey(flows, "msoa21cd", "year", "age", "sex")

full_backseries <- population[flows]

full_backseries <- deaths[full_backseries]

full_backseries <- births[full_backseries]

setkey(full_backseries, "msoa21cd")
setkey(msoa21_lad23, "msoa21cd")

full_backseries <- msoa21_lad23[full_backseries]

full_backseries <- full_backseries[, c("msoa21cd", "msoa21nm", "lad23cd", "lad23nm", 
                                       "year", "age", "sex", 
                                       "population", "births", "deaths", "net_flows", "inflow", "outflow")]


## 5. saving the final dataset
file_path <- paste0("output_data/revised_backseries_msoa21cd_2012_", max_year, ".rds")

saveRDS(object = full_backseries,
        file = file_path)
