## Fit SYA deaths by LSOA 11 for years to 2023. 
## This is simple apportionment from 5 year age band
## TODO: add proper IPF process using population at risk as seed values

## 0. libraries and functions
library(data.table)
library(gsscoder)
library(dplyr)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in the lsoa11 pop and death data, narrowing to years
lsoa_deaths <- data.table(readRDS("input_data/intermediate/deathsbylsoa11midyear01to23.rds"))

lsoa_deaths <- lsoa_deaths[year >= 2010 & year <= 2023, ]

## 2. reading in the la level mid year estimates, extracting the death components

### 2.1. reading in, extracting death data
mye_series <- data.table(readRDS("input_data/raw/adjusted_rebuilt_mye_backseries.rds"))

lad_deaths <- mye_series[component == "deaths" & year >= 2010 & year <= 2023, 
                         c("gss_code", "gss_name", "year", "age", "sex", "value")]

lad_deaths[value < 0, value := 0]

rm(mye_series)
gc()

age_lookup <- fread("lookups/age_band_lookup.csv") %>%
  select(age = sya, 
         age_group = lsoa_deaths) %>%
  distinct()

## 3. Allocating deaths by age and LSOA

lsoa_death_proportions <- lsoa_deaths %>%
  group_by(gss_code, year, sex, age_group) %>%
  mutate(group_deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(death_proportion = case_when(
    group_deaths > 0 ~ deaths/group_deaths, 
    TRUE ~ 0)) %>%
  select(lsoa11cd = LSOA11CD, gss_code, year, sex, age_group, death_proportion)

lsoa_deaths_sya <- lad_deaths %>%
  left_join(age_lookup, by = c("age")) %>%
  left_join(lsoa_death_proportions, by = c("gss_code", "sex", "age_group", "year")) %>%
  mutate(deaths = value * death_proportion) %>%
  select(lsoa11cd, year, age, sex, deaths)
  
## 4. saving the outputs
saveRDS(object = lsoa_deaths_sya,
        file = "input_data/intermediate/fitted_lsoa11_deaths.rds")

rm(list = ls())
gc()
gc()

