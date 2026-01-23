## calculates the final base gross flows, based on flows from census 2011, and then extended for the entire series up to the maximum year that we want to calculate the backseries for
## 2011 outflows and inflows were calculated in the previous script at lsoa11. This script just aggregates them to the final desired geography and extends it across all years according to the input weighted lookup. 


## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")


## 1. reading in data and lookups
gross_flows_lsoa <- readRDS("input_data/intermediate/base_gross_flows_lsoa11_2011.rds")

lsoa11_lookup_weighted <- readRDS(lsoa11_lookup_path)


## 2. getting base gross flows at the desired geography for the full series

  ### 2.1. aggregating lsoa11 up to the desired geography
gross_flows_lsoa <- gross_flows_lsoa[, .(year = min_year:max_year), # extending the base flows to go span the full time series in question
                                     by = list(lsoa11cd, sex, age, population_lsoa, inflow, outflow)]

gross_flows <- aggregate_geographies_weighted(data = gross_flows_lsoa, lookup = lsoa11_lookup_weighted,
                                              geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd", 
                                              geog_to_lookup = dest_geog_colname, count_names = c("population_lsoa", "inflow", "outflow"))

colnames(gross_flows)[colnames(gross_flows) == "population_lsoa"] <- "population"

  ### 2.2. subtracting flows within the geography, by estimating the total moves within the geography based on population and then scaling down by age and sex
gross_flows[, moved_within := estimate_moved_within(population)]

gross_flows <- scale_down_total_variable(dataset = gross_flows,
                                         variable_to_get_age_distribution = "inflow", variable_to_scale_down = "moved_within",
                                         categories_to_keep = c(dest_geog_colname, "year"))

gross_flows <- scale_down_total_variable(dataset = gross_flows,
                                         variable_to_get_age_distribution = "outflow", variable_to_scale_down = "moved_within",
                                         categories_to_keep = c(dest_geog_colname, "year"))

gross_flows[, inflow := inflow - moved_within_scaled_by_inflow]
gross_flows[, outflow := outflow - moved_within_scaled_by_outflow]

cols_to_keep <- c(dest_geog_colname, "year", "sex", "age", "inflow", "outflow")
gross_flows <- gross_flows[, ..cols_to_keep]

gross_flows[inflow <= 0, inflow := 0.1] # because for the optimise_gross_flows function in script 4_e, the input base gross flows can't have a value at or below 0
gross_flows[outflow <= 0, outflow := 0.1]


## 3. fixing up and saving the gross flows file
file_path <- paste0("input_data/intermediate/", "base_gross_flows_", dest_geog_colname, "_2011_", max_year, ".rds")

saveRDS(object = gross_flows,
        file = file_path)

rm(list = ls())
gc()



