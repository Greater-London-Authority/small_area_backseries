

## 0. libraries and functions, any inputs
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


## 1. reading files and lookups, fixing up data, writing out as pq

full_series_filepath <- paste0("output_data/revised_backseries_", dest_geog_colname, "_", min_year + 1, "_", max_year, ".rds")

full_series <- readRDS(full_series_filepath) %>%
  rename(area_code = all_of(dest_geog_colname)) %>% 
  rename(in_migration = inflow,
         out_migration = outflow,
         net_migration = net_flows) %>%
  mutate(geography = geography_name,
         scenario = scenario_name) %>%
  pivot_longer(cols = -any_of(c("area_code", "geography", "scenario", "year", "age", "sex")),
               names_to = "component",
               values_to = "value") 

population <- full_series %>% 
  filter(component == "population") %>% 
  replace_na(list(value = 0)) %>% 
  arrange(area_code, year, component, sex, age)

components <- full_series %>%
  filter(component != "population") %>%
  filter(year > 2011) %>%
  replace_na(list(value = 0)) %>%
  arrange(area_code, year, component, sex, age)

bind_rows(population, components) %>%
  write_dataset(path = paste0("output_data/estimates_backseries_", max_year),
                format = "parquet", 
                partitioning = c("geography", "component", "scenario", "year"))


rm(list = ls())
gc()


