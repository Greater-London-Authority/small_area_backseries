
## 0. libraries and functions
library(data.table)
library(gsscoder)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")

## 1. reading in the death data, narrowing to years
lsoa_deaths <- data.table(readRDS("input_data/intermediate/deathsbylsoa21midyear24.rds"))

lsoa_deaths <- lsoa_deaths[year >= 2024 & year <= max_year, ] # right now this data only has 2024 data, but it might be extended in future, and probably will be


## 2. reading in the la level mid year estimates, extracting the death components
mye_series <- data.table(readRDS("input_data/raw/population_estimates_gla_timeseries_2001_2024.rds"))

lad_deaths <- mye_series[component == "deaths" & year >= 2024 & year <= max_year, 
                         c("gss_code", "gss_name", "year", "age", "sex", "value")]

lad_deaths[value < 0, value := 0]

rm(mye_series)
gc()

## 3. converting the lad geographies to 2023 (2020 is recoded from 2021, everything else recoded from 2020. While for the lad data it's all from 2021)
## unfortunately, changes in 2025 for Barnsley and Sheffield that gsscoder hasn't been updated to be able to fix. So need to change it manually. TO CHECK - who is updating that now? Is just the python version being updated? 
## changing the two new 2025 lad codes back to 2023. Only way for now. 

lad_deaths <- recode_gss(df_in = lad_deaths[, -"gss_name"], 
                         col_code = "gss_code", 
                         col_data = "value",
                         recode_from_year = 2021,
                         recode_to_year = 2023)

lad_deaths <- data.table(lad_deaths)

lsoa_deaths[gss_code == "E08000038", gss_code := "E08000016"]
lsoa_deaths[gss_code == "E08000039", gss_code := "E08000019"]


## 3. reading in the lookups
age_lookup <- fread("lookups/age_band_lookup.csv")

age_lookup_lsoa <- unique(age_lookup[, c("lsoa_deaths", "sya")])


## 4. creating the seed - splitting age from 5 year age bands into single year of age

### 4.1. small bit of cleaning up on the dataset
colnames(lsoa_deaths) <- tolower(colnames(lsoa_deaths))

lsoa_deaths_21 <- lsoa_deaths[, c("lsoa21cd", "gss_code", "year", "age_group", "sex", "deaths")]

lsoa_deaths_21_formar <- copy(lsoa_deaths_21)


### 4.2. splitting the 5-year age bands into single year of age
setkey(age_lookup_lsoa, "lsoa_deaths")
setkey(lsoa_deaths_21, "age_group")

lsoa_deaths_21 <- lsoa_deaths_21[age_lookup_lsoa, allow.cartesian = TRUE]

divs <- data.table(table(age_lookup_lsoa[, "lsoa_deaths"]))

setkey(divs, "lsoa_deaths")
setkey(lsoa_deaths_21, "age_group")

lsoa_deaths_21 <- divs[lsoa_deaths_21]

lsoa_deaths_21[, deaths := deaths/N]

lsoa_deaths_21 <- lsoa_deaths_21[, c("lsoa21cd", "gss_code", "year", "sya", "sex", "deaths")]
colnames(lsoa_deaths_21)[2] <- "lad22cd"

lsoa_seed <- lsoa_deaths_21



## 5. creating the margins

### 5.1. lad21 by year by sya by sex
lad_deaths <- lad_deaths[, c("gss_code", "year", "age", "sex", "value")]
colnames(lad_deaths)[1] <- "lad22cd"

lad_deaths[age %in% c(85, 86, 87, 88, 89, 90), age := 85]

lad_deaths <- lad_deaths[, .(value = sum(value)), 
                         by = list(lad22cd, year, age, sex)]

lad_deaths_mar <- lad_deaths

### 5.2. lsoa21 by lad21 by year by sex
lsoa_deaths_mar <- lsoa_deaths_21_formar[, .(deaths = sum(deaths)),
                                         by = list(lsoa21cd, gss_code, year, sex)]

colnames(lsoa_deaths_mar)[2] <- "lad22cd"

rm(lsoa_deaths_21_formar)
gc()


### 5.4. scaling the lsoa figures so that the marginals add up (IPF doesn't work if the marginals give different totals)
### by getting the ratio difference between the two marginals by year and sex and lad22cd, and multiplying the lsoa values by these scaling factors
### this will lead to decimal points in the marginal for lsoas. I think this is ok, because they're just an input into creating estimates for deaths, which we can round later if we like. 
### this also means that I am assuming that the local authority level mid year estimates are the "correct" estimates and that we scale the lsoa estimates to match them. This may or may not be a sound assumption, but I don't think it matters very much as the differences are very small. 

lad_scale <- lad_deaths_mar[, .(deaths_lad = sum(value)),
                            by = list(lad22cd, year, sex)]

lsoa_scale <- lsoa_deaths_mar[, .(deaths_lsoa = sum(deaths)),
                              by = list(lad22cd, year, sex)]

setkey(lad_scale, "lad22cd", "year", "sex")
setkey(lsoa_scale, "lad22cd", "year", "sex")

scaling_factors <- lsoa_scale[lad_scale]

scaling_factors[, scaling_factors := deaths_lad/deaths_lsoa]

scaling_factors <- scaling_factors[, c("lad22cd", "year", "sex", "scaling_factors")]

setkey(scaling_factors, "lad22cd", "year", "sex")
setkey(lsoa_deaths_mar, "lad22cd", "year", "sex")

lsoa_deaths_mar <- scaling_factors[lsoa_deaths_mar]

lsoa_deaths_mar[, deaths := deaths*scaling_factors]
lsoa_deaths_mar <- lsoa_deaths_mar[, -"scaling_factors"]


### 5.5. adding values of 0 for cells that are missing
lsoa_seed_missing <- get_missing_categories(dataset = lsoa_seed, 
                                            cat_cols = c("lsoa21cd", "year", "sya", "sex"),
                                            value_col = "deaths")

lsoa_deaths_mar_missing <- get_missing_categories(dataset = lsoa_deaths_mar, 
                                                  cat_cols = c("year", "sex", "lsoa21cd"), 
                                                  value_col = "deaths") 

lsoa21_lad22 <- unique(lsoa_seed[, c("lsoa21cd", "lad22cd")])

setkey(lsoa21_lad22, "lsoa21cd")

setkey(lsoa_seed_missing, "lsoa21cd")
setkey(lsoa_deaths_mar_missing, "lsoa21cd")

lsoa_seed_missing <- lsoa21_lad22[lsoa_seed_missing]
lsoa_deaths_mar_missing <- lsoa21_lad22[lsoa_deaths_mar_missing]

length(unique(lsoa_deaths_mar_missing$lsoa21cd))
unique(lsoa_deaths_mar_missing$lsoa21cd)


lsoa_seed <- rbind(lsoa_seed, lsoa_seed_missing)
lsoa_deaths_mar <- rbind(lsoa_deaths_mar, lsoa_deaths_mar_missing)

lsoa_seed[is.na(deaths), deaths := 0]
lsoa_deaths_mar[is.na(deaths), deaths := 0]

## 6. saving the outputs
saveRDS(object = lsoa_seed,
        file = "input_data/intermediate/lsoa21_seed.rds")


saveRDS(object = lad_deaths_mar,
        file = "input_data/intermediate/lad_deaths_margin_lsoa21.rds")


saveRDS(object = lsoa_deaths_mar,
        file = "input_data/intermediate/lsoa21_deaths_margin.rds")


rm(list = ls())
gc()
gc()
