## NOTE - script won't work automatically with new geographies, because of the lad lookup
## but why are we adding lad codes in the first place? 

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

geography_name <- "ward22"
scenario_name <- "adjusted"


## 1. reading files and lookups, fixing up data, writing out as pq

lookup_wd_lad <- readRDS("lookups/ward22_lad23.rds") %>% 
  select(area_code = wd22cd, gss_code = lad23cd)

lookup_wd_lad <- unique(lookup_wd_lad)

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
  write_dataset(path = paste0("output_data/estimates_backseries_", max_year),
                format = "parquet", 
                partitioning = c("geography", "component", "scenario", "year"))


rm(list = ls())
gc()



