
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
flows <- readRDS(paste0("input_data/intermediate/estimated_gross_flows_ward22_", min_year, "_", max_year, ".rds"))

population <- readRDS(paste0("input_data/intermediate/mid_year_rebased_2011", max_year, "_ward22.rds"))

deaths <- readRDS("input_data/intermediate/fitted_lsoa11_deaths.rds")

deaths_24 <- readRDS("input_data/intermediate/fitted_lsoa21_deaths.rds")

births <- readRDS("input_data/intermediate/births_2001_2024_oa21.rds")


### 1.2. lookups
lsoa11_ward22_weighted <- readRDS("lookups/lsoa11_ward22_weighted_extended.rds")

lsoa21_ward22 <- readRDS("lookups/lsoa21_ward22_bf.rds")

oa21_ward22 <- readRDS("lookups/oa21_ward22_bf.rds")


## 2. aggregating all geographies to ward22cd and to common age categories

### 2.1. population (already at ward)
population <- population[year <= max_year, -"ward22nm"]

population <- aggregate_to_lower_max_age(input_data = population, new_max_age = 85, count_names = "population")

### 2.2. deaths
deaths <- aggregate_geographies_weighted(data = deaths[year >= min_year, ], lookup = lsoa11_ward22_weighted,
                                         geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd",
                                         geog_to_lookup = "ward22cd", count_name = "deaths")

deaths_24 <- aggregate_geographies_2(data = deaths_24, lookup = lsoa21_ward22,
                                     geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd", geog_to_lookup = "ward22cd",
                                     count_names = "deaths")

if(max_year >= 2024){
  
  deaths <- rbind(deaths, 
                  deaths_24)
  
}

### 2.3. births
births <- births[year >= min_year & year <= max_year, ]

births <- aggregate_geographies_2(data = births, lookup = oa21_ward22, # NOTE - be aware that I updated the process for preparing the births data. Quite sure that the data is correct and in the same format, but in case something goes wrong check here. 
                                  geog_from_data = "oa21cd", geog_from_lookup = "oa21cd",
                                  geog_to_lookup = "ward22cd", count_names = "births")

births[, age := 0]

### 2.4. should have a separate section here I feel for filtering by year, rather than doing it in the section above in a slightly scattered way. A bit neater/clearer. 
### also, overall, the script to this exact point here is what I need to convert to general functions with respect to years and geography to make the whole thing flexible. Also need to change geography labels below, but that's a lot easier.


## 4. joining the datasets, adding on la codes
setkey(population, "ward22cd", "year", "age", "sex")
setkey(births, "ward22cd", "year", "age", "sex")
setkey(deaths, "ward22cd", "year", "age", "sex")
setkey(flows, "ward22cd", "year", "age", "sex")

full_backseries <- population[flows]

full_backseries <- deaths[full_backseries]

full_backseries <- births[full_backseries]

full_backseries <- full_backseries[, c("lad23cd", "ward22cd", # add ward and lad names? 
                                       "year", "age", "sex", 
                                       "population", "births", "deaths", "net_flows", "inflow", "outflow")]


## 5. saving the final dataset
file_path <- paste0("output_data/revised_backseries_ward22cd_2012_", max_year, ".rds")

saveRDS(object = full_backseries,
        file = file_path)

