## getting all of the components together into one file 

## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

## 1. reading in data

  ### 1.1. population estimates and components of change
population <- fread("input_data/intermediate/mid_year_rebased_20112022_lsoa21.csv")

births <- readRDS("input_data/raw/births_oa11_mid_2002_2020.rds")

deaths <- readRDS("input_data/intermediate/fitted_lsoa_deaths.rds")

flows <- readRDS("input_data/intermediate/estimated_gross_flows_2012_2020.rds")

  ### 1.2. lookups
oa11_lsoa21_lookup <- fread("lookups/2011_oa_2022_lsoa_msoa_la.csv") # all of below can be done, and should be done, in a function. The joining and aggregating, that is. 

oa11_lsoa21_lookup <- oa11_lsoa21_lookup[, c("oa11cd", "lsoa21cd")] # hmm. Seems that this lookup is missing 157 lsoas. Out of over 180,000. Look into how much this matters, and what we should do about it - TO DISCUSS. 


## 2. aggregating births up to lsoa21
births <- data.table(births)

setkey(oa11_lsoa21_lookup, "oa11cd")
setkey(births, "OA11CD")

births <- oa11_lsoa21_lookup[births]

births <- births[, .(births = sum(births)),
                 by = list(lsoa21cd, sex, year)]


## 3. a few small fixes to each dataset before joining
population <- population[year >= 2012 & year <= 2020, -c("lad21cd", "lad21nm")]

population <- aggregate_to_lower_max_age(input_data = population, new_max_age = 75, count_names = "population")

births[, age := 0]

births <- births[year >= 2012 & year <= 2020, ]

deaths <- aggregate_to_lower_max_age(input_data = deaths, new_max_age = 75, count_names = "deaths")

deaths <- deaths[year >= 2012 & year <= 2020, -"lad23cd"]


## 4. joining the datasets
setkey(population, "lsoa21cd", "year", "age", "sex")
setkey(births, "lsoa21cd", "year", "age", "sex")
setkey(deaths, "lsoa21cd", "year", "age", "sex")
setkey(flows, "lsoa21cd", "year", "age", "sex")

## note to self - join deaths around the others, because that is the one that is currently missing some lsoas. Won't matter when we've come up with what to do about that issue though. 

full_backseries <- population[flows]

full_backseries <- deaths[full_backseries]

full_backseries <- births[full_backseries]

full_backseries <- full_backseries[, c("lad23cd", "lsoa21cd", "lsoa21nm", "year", "age", "sex", 
                                       "population", "births", "deaths", "net_flows", "inflow", "outflow")]

## 5. saving the final dataset
saveRDS(object = full_backseries,
        file = "output_data/revised_backseries_lsoa21cd_2012_2020.rds")


