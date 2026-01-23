## creates the inputs needed - the seed and the margin table - to feed into IPF to create single year of age death estimates on the lsoa21 boundaries
## general with respect to max_year. Update max_year and the script will run. 
## general for what type of la boundaries we're working on too. Aside from one big exception - the lsoa deaths data has been released on 2025 boundaries, and gss coder hasn't been updated to accommodate those yet. So that, at the moment, is updated manually, which isn't very robust. 
## TO RAISE - gss coder needs to be updated to be able to handle 2025 boundaries. 

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
lsoa_deaths <- data.table(readRDS(paste0("input_data/intermediate/deathsbylsoa21midyear_2024_to_", max_year, ".rds")))

lsoa_deaths <- lsoa_deaths[year >= 2024 & year <= max_year, ]


## 2. reading in the la level mid year estimates, extracting the death components
### can't read it directly off datastore, I think because of the changes that have been made recently. Also don't think that ldnatar package is ready yet. So downloading as tempfile is, I think, the easiest way to do this for now. 

mye_url <- latest_gla_mye_url
tmp <- tempfile(fileext = ".rds")
download.file(mye_url, tmp, mode = "wb")
mye_series <- data.table(readRDS(tmp))

lad_deaths <- mye_series[component == "deaths" & year >= 2024 & year <= max_year, 
                         c("gss_code", "gss_name", "year", "age", "sex", "value")]

lad_deaths[value < 0, value := 0]

rm(mye_series)
gc()


## 3. converting the lad geographies to 2023
## unfortunately, there are changes in 2025 for Barnsley and Sheffield that gsscoder hasn't been updated to be able to fix. So need to change it manually. TO CHECK - who is updating that now? Is just the python version being updated? 
## the above is true just for the new deaths by lsoa by 5-year age bands series. The lad series is fine.
## changing the two new 2025 lad codes back to 2023 in the lsoa dataset manually. Best way for now. 

lad_recode_from <- get_gss_year(df_in = lad_deaths, col_code = "gss_code")

lad_deaths <- recode_gss(df_in = lad_deaths[, -"gss_name"], 
                         col_code = "gss_code", 
                         col_data = "value",
                         recode_from_year = lad_recode_from,
                         recode_to_year = lad_version)

lad_deaths <- data.table(lad_deaths)

lsoa_deaths[gss_code == "E08000038", gss_code := "E08000016"]
lsoa_deaths[gss_code == "E08000039", gss_code := "E08000019"]


## 4. reading in the lookups
age_lookup <- fread("lookups/age_band_lookup.csv")

age_lookup_lsoa <- unique(age_lookup[, c("lsoa_deaths", "sya")])


## 5. creating the seed - splitting age from 5 year age bands into single year of age

  ### 5.1. small bit of cleaning up on the dataset
colnames(lsoa_deaths) <- tolower(colnames(lsoa_deaths))

lsoa_deaths_21 <- lsoa_deaths[, c("lsoa21cd", "gss_code", "year", "age_group", "sex", "deaths")]

lsoa_deaths_21_formar <- copy(lsoa_deaths_21)

  ### 5.2. splitting the 5-year age bands into single year of age (again, should make a function that does this)
setkey(age_lookup_lsoa, "lsoa_deaths")
setkey(lsoa_deaths_21, "age_group")

lsoa_deaths_21 <- lsoa_deaths_21[age_lookup_lsoa, allow.cartesian = TRUE]

divs <- data.table(table(age_lookup_lsoa[, "lsoa_deaths"]))

setkey(divs, "lsoa_deaths")
setkey(lsoa_deaths_21, "age_group")

lsoa_deaths_21 <- divs[lsoa_deaths_21]

lsoa_deaths_21[, deaths := deaths/N]

lsoa_deaths_21 <- lsoa_deaths_21[, c("lsoa21cd", "gss_code", "year", "sya", "sex", "deaths")]

lsoa_deaths_21[, gss_type := paste0("lad_", lad_version)] # I don't think we ever fully decided what we could call the geography columns if we really do need more than one geographic level, which is the case here. So assuming that gss_type is meant to describe the highest of geography, lad, and then keeping lsoa as the column heading for that level.

lsoa_seed <- copy(lsoa_deaths_21)


## 6. creating the margins

  ### 6.1. lad by year by sya by sex
lad_deaths[, gss_type := paste0("lad_", lad_version)]

lad_deaths <- lad_deaths[, c("gss_code", "gss_type", "year", "age", "sex", "value")]

lad_deaths_mar <- copy(lad_deaths)

  ### 6.2. lsoa21 by lad by year by sex
lsoa_deaths_mar <- lsoa_deaths_21_formar[, .(deaths = sum(deaths)),
                                         by = list(lsoa21cd, gss_code, year, sex)]

rm(lsoa_deaths_21_formar)
gc()

  ### 6.3. scaling the lsoa figures so that the margins add up (IPF doesn't work if the margins give different totals)
  ### by getting the ratio difference between the two margins by year and sex and lad22cd, and multiplying the lsoa values by these scaling factors
  ### this will lead to decimal points in the margin for lsoas. I think this is ok, because they're just an input into creating estimates for deaths, which we can round later if we like. 
  ### this also means that I am assuming that the local authority-level mid-year estimates are the "correct" estimates and that we scale the lsoa estimates to match them. This may or may not be a sound assumption, but I don't think it matters very much as the differences are very small. 

lad_scale <- lad_deaths_mar[, .(deaths_lad = sum(value)),
                            by = list(gss_code, gss_type, year, sex)]

lsoa_scale <- lsoa_deaths_mar[, .(deaths_lsoa = sum(deaths)),
                              by = list(gss_code, year, sex)]

setkey(lad_scale, "gss_code", "year", "sex")
setkey(lsoa_scale, "gss_code", "year", "sex")

scaling_factors <- lsoa_scale[lad_scale]

scaling_factors[, scaling_factors := deaths_lad/deaths_lsoa]

scaling_factors <- scaling_factors[, c("gss_code", "gss_type", "year", "sex", "scaling_factors")]

setkey(scaling_factors, "gss_code", "year", "sex")
setkey(lsoa_deaths_mar, "gss_code", "year", "sex")

lsoa_deaths_mar <- scaling_factors[lsoa_deaths_mar]

lsoa_deaths_mar[, deaths := deaths*scaling_factors]
lsoa_deaths_mar <- lsoa_deaths_mar[, -"scaling_factors"]

  ### 6.4. adding values of 0 for cells that are missing
lsoa_seed_missing <- get_missing_categories(dataset = lsoa_seed, 
                                            cat_cols = c("lsoa21cd", "year", "sya", "sex"),
                                            value_col = "deaths")

lsoa_deaths_mar_missing <- get_missing_categories(dataset = lsoa_deaths_mar, 
                                                  cat_cols = c("year", "sex", "lsoa21cd"), 
                                                  value_col = "deaths") 

lsoa21_lad <- unique(lsoa_seed[, c("lsoa21cd", "gss_code", "gss_type")])

setkey(lsoa21_lad, "lsoa21cd")

setkey(lsoa_seed_missing, "lsoa21cd")
setkey(lsoa_deaths_mar_missing, "lsoa21cd")

lsoa_seed_missing <- lsoa21_lad[lsoa_seed_missing]
lsoa_deaths_mar_missing <- lsoa21_lad[lsoa_deaths_mar_missing]

lsoa_seed <- rbind(lsoa_seed, lsoa_seed_missing)
lsoa_deaths_mar <- rbind(lsoa_deaths_mar, lsoa_deaths_mar_missing)

lsoa_seed[is.na(deaths), deaths := 0]
lsoa_deaths_mar[is.na(deaths), deaths := 0]


## 7. saving the outputs
saveRDS(object = lsoa_seed,
        file = paste0("input_data/intermediate/lsoa21_seed", "_to_", max_year, ".rds"))

saveRDS(object = lad_deaths_mar,
        file = paste0("input_data/intermediate/lad_deaths_margin_lsoa21", "_to_", max_year, ".rds"))

saveRDS(object = lsoa_deaths_mar,
        file = paste0("input_data/intermediate/lsoa21_deaths_margin", "_to_", max_year, ".rds"))


rm(list = ls())
gc()
gc()

