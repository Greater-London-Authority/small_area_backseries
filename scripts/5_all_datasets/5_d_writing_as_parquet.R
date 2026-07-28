
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

geography_name <- "ward22"
scenario_name <- "adjusted"


## 1. reading files and lookups (and lots of data wrangling and processing as soon as we read the file in...)

lookup_wd_lad <- readRDS("lookups/ward22_lad23.rds") %>%
  select(area_code = gss_code_ward, gss_code)

full_series_filepath <- paste0("output_data/revised_backseries_", dest_geog_colname, "_", min_year + 1, "_", max_year, ".rds")

full_series <- readRDS(full_series_filepath) %>%
  rename(area_code = ward22cd) %>%
  rename(in_migration = inflow,
         out_migration = outflow,
         net_migration = net_flows) %>%
  mutate(geography = geography_name,
         scenario = scenario_name) %>%
  left_join(lookup_wd_lad, by = c("area_code")) %>%
  pivot_longer(cols = -any_of(c("gss_code", "area_code", "geography", "scenario", "year", "age", "sex")),
               names_to = "component",
               values_to = "value") 

population <- full_series %>%
  filter(component == "population") %>%
  replace_na(list(value = 0)) %>%
  arrange(gss_code, area_code, year, component, sex, age)

components <- full_series %>%
  filter(component != "population") %>%
  filter(year > 2011) %>%
  replace_na(list(value = 0)) %>%
  arrange(gss_code, area_code, year, component, sex, age)

bind_rows(population, components) %>%
  write_dataset(path = "output_data/estimates_backseries", # new system for filepath saving, after reviewing and testing
                format = "parquet", 
                partitioning = c("geography", "component", "scenario", "year"))





