## this is an ad hoc script, at the end of the process, to add on local authority codes to the final outputs
## requires some manual changing etc, not a core script

## 0. libraries and functions
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


## 1. reading in the estimates, rates, and lookups

estimates_parquet <- open_dataset(paste0("output_data/estimates_backseries_", max_year))
rates_parquet <- open_dataset(paste0("output_data/input_rates_", max_year))

estimates <- estimates_parquet %>%
  filter(
    geography == geography_name,
  ) %>%
  collect() %>%
  data.table() # too big - data.table much quicker

rates <- rates_parquet %>%
  filter(
    geography == geography_name,
  ) %>%
  collect()  %>%
  data.table() # also fairly big, but using data.table more for consistency with above. 


lookup <- fread("lookups/msoa21_lad23.csv")

colnames(lookup) <- tolower(colnames(lookup))

lookup <- lookup[, c("msoa21cd", "lad23cd")]


## 2. joining on the codes, some dataset cleaning

estimates <- lookup[estimates, on = c("msoa21cd" = "area_code")]
rates <- lookup[rates, on = c("msoa21cd" = "area_code")]


setnames(estimates, old = c("msoa21cd", "lad23cd"), new = c("area_code", "gss_code"))
setnames(rates, old = c("msoa21cd", "lad23cd"), new = c("area_code", "gss_code"))



## 3. reading back out the pq files

write_dataset(dataset = estimates,
              path = paste0("output_data/estimates_backseries_", max_year), # just overwriting the existing version - the exact same output aside from the new lad codes
              format = "parquet", 
              partitioning = c("geography", "component", "scenario", "year"))


write_dataset(dataset = rates,
              path = paste0("output_data/input_rates_", max_year), # just overwriting the existing version - the exact same output aside from the new lad codes
              format = "parquet", 
              partitioning = c("geography", "component", "scenario", "year"))
