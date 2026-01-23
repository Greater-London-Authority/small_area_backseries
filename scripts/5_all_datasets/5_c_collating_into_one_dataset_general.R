## simple script. Takes in the population data and all components of change, aggregates them to the desired final geography, and combines them into one dataset


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
flows <- readRDS(paste0("input_data/intermediate/estimated_gross_flows_", dest_geog_colname, "_", min_year, "_", max_year, ".rds"))

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

oa21_lookup_extended <- extend_lookup_and_add_weight(oa21_lookup, year_start = 2011, year_end = 2024) # for best fit lookups, converting them into "weighted" lookups by year, with all weights equal to 1
lsoa21_lookup_extended <- extend_lookup_and_add_weight(lsoa21_lookup, year_start = 2011, year_end = 2024)


## 2. aggregating all geographies to destination geography and to common age categories

  ### 2.1. population
population <- population[year <= max_year, -"lsoa21nm"]

population <- aggregate_geographies_weighted(data = population, lookup = lsoa21_lookup_extended,
                                             geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd",
                                             geog_to_lookup = dest_geog_colname, count_names = "population")

  ### 2.2. deaths
deaths <- aggregate_geographies_weighted(data = deaths[year >= min_year, ], lookup = lsoa11_lookup_weighted,
                                         geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd",
                                         geog_to_lookup = dest_geog_colname, count_name = "deaths")

deaths_24_onward <- aggregate_geographies_weighted(data = deaths_24_onward, lookup = lsoa21_lookup_extended,
                                                   geog_from_data = "lsoa21cd", geog_from_lookup = "lsoa21cd", 
                                                   geog_to_lookup = dest_geog_colname, count_names = "deaths")


if(max_year >= 2024){
  
  deaths <- rbind(deaths, 
                  deaths_24_onward)
  
}

  ### 2.3. births
births <- births[year >= min_year & year <= max_year, ]

births <- aggregate_geographies_weighted(data = births, lookup = oa21_lookup_extended, 
                                         geog_from_data = "oa21cd", geog_from_lookup = "oa21cd",
                                         geog_to_lookup = dest_geog_colname, count_names = "births")

births[, age := 0]


## 3. joining the datasets
join_cols <- c(dest_geog_colname, "year", "age", "sex")

setkeyv(population, join_cols)
setkeyv(births, join_cols)
setkeyv(deaths, join_cols)
setkeyv(flows, join_cols)

full_backseries <- population[flows]

full_backseries <- deaths[full_backseries]

full_backseries <- births[full_backseries]

col_ords <- c(dest_geog_colname, 
              "year", "age", "sex", 
              "population", "births", "deaths", "net_flows", "inflow", "outflow")

full_backseries <- full_backseries[, ..col_ords]


## 4. saving the final dataset
full_backseries <- full_backseries[!(is.na(get(dest_geog_colname)) & population == 0), ] ## in this way, we'd keep the rows where there are actual population values and a geography code of NA. That would mean that something had gone wrong and we'd need to manually check.

file_path <- paste0("output_data/revised_backseries_", dest_geog_colname, "_", min_year + 1, "_", max_year, ".rds")

saveRDS(object = full_backseries,
        file = file_path)

rm(list = ls())
gc()
gc()





