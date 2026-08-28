

## 0. libraries and functions
library(nomisr)
library(data.table)
library(parallel)
library(gsscoder)

source("scripts/inputs.R")


functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in datasets and lookups

la_level_backseries <- data.table(readRDS("input_data/raw/adjusted_population_lad.rds")) # for now I've just dropped this right into the raw folder, from the repo that created the la-level revised backseries with adjusted young population. Once that process is finalised and it's on the datastore, change this so that it reads it in directly. The link is already set in the inputs script, in the object latest_gla_mye_url. Although the ability to read it in may not have survived the various changes to the datastore.     
gsscoder::get_gss_year(la_level_backseries)

lsoa_population <- readRDS(paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_lsoa21.rds"))

lsoa_la_lookup <- fread("lookups/lsoa21_lad23.csv")

lsoa_code_name_lookup <- unique(lsoa_population[, c("lsoa21cd", "lsoa21nm")]) # needed for later


## 2. creating the scaling factors

  ### 2.1.  # aggregating the lsoa estimates to lad level
lsoa_population <- lsoa_population[, -"lsoa21nm"]

la_pop_from_lsoa <- aggregate_geographies_2(data = lsoa_population,
                                lookup = lsoa_la_lookup, 
                                geog_from_data = "lsoa21cd",
                                geog_from_lookup = "lsoa21cd",
                                geog_to_lookup = "lad23cd",
                                count_names = "population")

la_level_backseries <- la_level_backseries[component == "population", -"component"]

la_level_backseries[value < 0, value := 0]

  ### 2.2. joining the lad estimates derived from aggregating lsoa estimates with the revised and adjusted backseries
setnames(la_pop_from_lsoa, old = "population", new = "pop_from_lsoa") 
setnames(la_level_backseries, old = "value", new = "pop_from_backseries")

setkey(la_pop_from_lsoa, "lad23cd", "year", "age", "sex")
setkey(la_level_backseries, "gss_code", "year", "age", "sex")

scaling_factors <- la_level_backseries[la_pop_from_lsoa]

scaling_factors[, scaling_factor := pop_from_backseries/pop_from_lsoa]

scaling_factors[!is.finite(scaling_factor), scaling_factor := 1]

scaling_factors <- scaling_factors[scaling_factors, c("gss_code", "sex", "year", "age", "scaling_factor")]


## 3. apply the scaling factors to the lsoa population estimates, to constrain them to the adjusted population estimates

lsoa_population <- lsoa_la_lookup[lsoa_population, on = "lsoa21cd"] # joining on lad codes

setkey(scaling_factors, "gss_code", "sex", "year", "age") # joining the scaling factors on to the lsoa-level population estimates
setkey(lsoa_population, "lad23cd", "sex", "year", "age")

lsoa_population <- scaling_factors[lsoa_population]

lsoa_population[, population := scaling_factor*population] # applying the scaling factors


## 4. fixing up the format of the dataset and saving the output

lsoa_population <- lsoa_code_name_lookup[lsoa_population, on = "lsoa21cd"]

lsoa_population <- lsoa_population[, c("year", "lsoa21nm", "lsoa21cd", "age", "sex", "population")]

file_path <- paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_lsoa21.rds") # saving it with the same file path as before - the same dataset for the same purposes, with amendments to the values

saveRDS(object = lsoa_population,
        file_path)


