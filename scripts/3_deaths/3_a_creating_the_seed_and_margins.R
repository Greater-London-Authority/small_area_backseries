

## 0. libraries and functions
library(data.table)
library(gsscoder)

## 1. reading in the death data, narrowing to years we need
lsoa_deaths <- data.table(readRDS("input_data/raw/deathsbylsoamidyear01to20.rds"))

lsoa_deaths <- lsoa_deaths[year <= 2020 & year >= 2011, ]


## 2. reading in the la level mid year estimates, extracting the death components
download.file("https://data.london.gov.uk/download/fb203828-bde5-4a50-96d9-8adfb4960631/ba752f34-0b54-4184-9251-8e2e94ae97ee/full_modelled_estimates_series_EW(2023_geog).rds", # should probably be done earlier, in the population folder
              "input_data/raw/population_estimates_gla_timeseries_2001_2024.rds",
              mode = "wb")

mye_series <- data.table(readRDS("input_data/raw/population_estimates_gla_timeseries_2001_2024.rds"))

lad_deaths <- mye_series[component == "deaths" & year <= 2020 & year >= 2011, 
                         c("gss_code", "gss_name", "year", "age", "sex", "value")]

lad_deaths[value < 0, value := 0]


## 3. reading in the lookups
oa11_lsoa11_lad22 <- fread("lookups/2011_oa_lsoa_msoa_lad.csv")
oa11_lsoa21_lad22 <- fread("lookups/2011_oa_2022_lsoa_msoa_la.csv")

oa11_lsoa11 <- unique(oa11_lsoa11_lad22[, c("oa11cd", "lsoa11cd")])
oa11_lsoa22 <- unique(oa11_lsoa21_lad22[, c("oa11cd", "lsoa21cd")])

age_lookup <- fread("lookups/age_band_lookup.csv")

age_lookup_lsoa <- unique(age_lookup[, c("lsoa_deaths", "sya")])


## 4. creating the seed - splitting lsoa11 to oa11, then aggregating that up to lsoa21, then splitting age from 5 year age bands into single year of age

### 4.1. converting lsoa11 to lsoa21, by splitting down to oa11 and then aggregating back up again (TO CHECK/DISCUSS - is the best way to do it?)
setkey(lsoa_deaths, "LSOA11CD")
setkey(oa11_lsoa11, "lsoa11cd")

lsoa_oa_deaths <- lsoa_deaths[oa11_lsoa11, allow.cartesian = TRUE]

lsoa_oa_deaths <- lsoa_oa_deaths[!is.na(year), ] # getting rid of ones that were in the lookup but not in the dataset. Deaths in Northern Ireland and so on. 

divs <- data.table(table(oa11_lsoa11[, "lsoa11cd"])) # TO DO - need a function for splitting a category into smaller constituent categories, which is what the below is doing

setkey(divs, "lsoa11cd")
setkey(lsoa_oa_deaths, "LSOA11CD")

lsoa_oa_deaths <- divs[lsoa_oa_deaths]

lsoa_oa_deaths[, deaths := deaths/N] 

oa_deaths_est <- lsoa_oa_deaths[, c("oa11cd", "gss_code", "year", "age_group", "sex", "deaths")]

setkey(oa_deaths_est, "oa11cd")
setkey(oa11_lsoa22, "oa11cd")

lsoa_deaths_21_est <- oa11_lsoa22[oa_deaths_est]

rm(oa_deaths_est)
rm(lsoa_oa_deaths)
gc()

lsoa_deaths_21_est <- lsoa_deaths_21_est[, .(deaths = sum(deaths)),
                                         by = list(lsoa21cd, gss_code, year, age_group, sex)]

lsoa_deaths_21_formar <- copy(lsoa_deaths_21_est)


### 4.2. splitting the 5-year age bands into single year of age (again, once I make a function to do this, this will be more compact and in a function)
setkey(age_lookup_lsoa, "lsoa_deaths")
setkey(lsoa_deaths_21_est, "age_group")

lsoa_deaths_21_est <- lsoa_deaths_21_est[age_lookup_lsoa, allow.cartesian = TRUE]

divs <- data.table(table(age_lookup_lsoa[, "lsoa_deaths"]))

setkey(divs, "lsoa_deaths")
setkey(lsoa_deaths_21_est, "age_group")

lsoa_deaths_21_est <- divs[lsoa_deaths_21_est]

lsoa_deaths_21_est[, deaths := deaths/N]

colnames(lsoa_deaths_21_est)

lsoa_deaths_21_est <- lsoa_deaths_21_est[, c("lsoa21cd", "gss_code", "year", "sya", "sex", "deaths")]
colnames(lsoa_deaths_21_est)[2] <- "lad22cd"


### 4.3. converting the seed table to lad23 geographies
### NOTE - it would be more efficient to convert the inputs to the correct geographies as soon as I read them in, instead of doing it several times bit by bit on the various outputs that come from that original input. Do this later. 

lsoa_seed_1119 <- recode_gss(df_in = lsoa_deaths_21_est[year %in% 2011:2019, ], 
                             col_code = "lad22cd", 
                             col_data = "deaths", 
                             recode_from_year = 2020, 
                             recode_to_year = 2023)

lsoa_seed_20 <- recode_gss(df_in = lsoa_deaths_21_est[year == 2020, ], 
                           col_code = "lad22cd", 
                           col_data = "deaths", 
                           recode_from_year = 2021, 
                           recode_to_year = 2023)

lsoa_seed <- rbind(data.table(lsoa_seed_1119),
                   data.table(lsoa_seed_20))

rm(lsoa_seed_1119, lsoa_seed_20, lsoa_deaths_21_est)

gc()


## 5. creating the marginals

  ### 5.1. lad21 by year by sya by sex
lad_deaths <- lad_deaths[, c("gss_code", "year", "age", "sex", "value")]
colnames(lad_deaths)[1] <- "lad22cd"

lad_deaths[age > 85, age := 85] # have created a function for this aggregation to new max ages - convert this to function

lad_deaths <- lad_deaths[, .(value = sum(value)), 
                         by = list(lad22cd, year, age, sex)]

lad_deaths_mar <- lad_deaths

  ### 5.2. lsoa21 by lad21 by year by sex
lsoa_deaths_mar <- lsoa_deaths_21_formar[, .(deaths = sum(deaths)), 
                                         by = list(lsoa21cd, gss_code, year, sex)]

colnames(lsoa_deaths_mar)[2] <- "lad22cd"

rm(lsoa_deaths_21_formar)
gc()

  ### 5.3. converting them both to lad23
lsoa_deaths_mar_1119 <- recode_gss(df_in = lsoa_deaths_mar[year %in% 2011:2019, ], 
                                   col_code = "lad22cd", 
                                   col_data = "deaths", 
                                   recode_from_year = 2020, 
                                   recode_to_year = 2023)

lsoa_deaths_mar_20 <- recode_gss(df_in = lsoa_deaths_mar[year == 2020, ], 
                                 col_code = "lad22cd", 
                                 col_data = "deaths", 
                                 recode_from_year = 2021, 
                                 recode_to_year = 2023)

lsoa_deaths_mar <- rbind(data.table(lsoa_deaths_mar_1119),
                         data.table(lsoa_deaths_mar_20))

lad_deaths_mar <- recode_gss(df_in = lad_deaths_mar, 
                             col_code = "lad22cd", 
                             col_data = "value",
                             recode_from_year = 2021,
                             recode_to_year = 2023)

lad_deaths_mar <- data.table(lad_deaths_mar)


  ### 5.4. scaling the lsoa figures so that the marginals add up (IPF doesn't work if the marginals give different totals)
  ### by getting the ratio difference between the two marginals by year and sex and lad22cd, and multiplying the lsoa values by these scaling factors
  ### this will lead to decimal points in the margin for lsoas. I think this is ok, in part because we already have decimals after estimating 2021 lsoas from 2011 lsoas. 
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


## 6. saving the outputs
saveRDS(object = lsoa_seed,
        file = "input_data/intermediate/lsoa_seed.rds")

saveRDS(object = lad_deaths_mar,
        file = "input_data/intermediate/lad_deaths_margin.rds")

saveRDS(object = lsoa_deaths_mar,
        file = "input_data/intermediate/lsoa_deaths_margin.rds")

rm(list = ls())
gc()
gc()

