## this is an ad-hoc script to create a new set of population estimates that adjusts for the undercount of young children seen in the 2021 census 
## it takes in an amended version of the la-level modelled backseries that accounts for the undercount and adjusts the estimates, and constrains the ward estimates to those la-level estimates
## the adjusted la-level backseries is created elsewhere and simply dropped into the folder. Should fix that...

# 0. libraries and functions
library(data.table)

source("scripts/inputs.R")


# 1. reading in the data and lookups
ward22_population <- readRDS(paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_ward22.rds"))

lad23_population <- data.table(readRDS('input_data/modelled_series_2011_24(2023_geog).rds'))

ward22_lad23_lookup <- readRDS("lookups/ward22_lad23.rds")


# 2. calculating the scaling rates
setkey(ward22_population, "ward22cd")
setkey(ward22_lad23_lookup, "wd22cd")

ward22_population <- ward22_population[ward22_lad23_lookup]

scaling_factors <- ward22_population[, .(population = sum(population)),
                  by = .(year, lad23cd, age, sex)]

colnames(scaling_factors)[colnames(scaling_factors) == "population"] <- "ward_population" 

lad23_population <- lad23_population[component == "population", ]

setkey(scaling_factors, "lad23cd", "year", "age", "sex")
setkey(lad23_population, "gss_code", "year", "age", "sex")

scaling_factors <- scaling_factors[lad23_population]
colnames(scaling_factors)[colnames(scaling_factors) == "value"] <- "lad_population"

scaling_factors[, scaling_factor := lad_population/ward_population]

scaling_factors <- scaling_factors[, c("year", "lad23cd", "age", "sex", "scaling_factor")]


# 3. scaling the ward population estimates to the lad population estimates
setkey(scaling_factors, "year", "lad23cd", "age", "sex")
setkey(ward22_population, "year", "lad23cd", "age", "sex")

ward22_population <- scaling_factors[ward22_population]
ward22_population <- ward22_population[!(is.na(population)), ] # getting rid of nas, which is mostly 2011 and a small number of wards outside london....
ward22_population <- ward22_population[!(is.na(lad23cd)), ] # the OR operator doesn't seem to work on mac? Hmm. 

ward22_population[, population := population*scaling_factor]


# 4. cleaning up and saving
ward22_population <- ward22_population[, c("year", "ward22cd", "age", "sex", "population")]

file_path <- paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_ward22.rds")

saveRDS(object = ward22_population,
        file_path)

rm(list = ls())

gc()
gc()
