## for creating net flows at msoa11 level for 2011, we need deaths at single year of age for msoa11. Meaning we need to do another whole run of the ipf process.
## going to create them at lsoa11, which will then perfectly aggregate up to msoa11. 
## also, we only need this for 2011 
## basically just copied and amended scripts 3_a and 3_b. Put some work into making it more compact etc. 

## 0. libraries and functions
library(data.table)
library(gsscoder)


## 1. reading in the death data, narrowing to years
lsoa_deaths <- data.table(readRDS("input_data/raw/deathsbylsoamidyear01to20.rds"))

lsoa_deaths <- lsoa_deaths[year == 2011, ]


## 2. reading in the la level mid year estimates, extracting the death components
mye_series <- data.table(readRDS("Q:/Teams/D&PA/Data/population_estimates/gla_timeseries/gla_timeseries_2001_2021.rds"))

lad_deaths <- mye_series[component == "deaths" & year == 2011, 
                         c("gss_code", "gss_name", "year", "age", "sex", "value")]

lad_deaths[value < 0, value := 0]


## 3. reading in the lookups
oa11_lsoa11_lad22 <- fread("lookups/2011_oa_lsoa_msoa_lad.csv")

oa11_lsoa11 <- unique(oa11_lsoa11_lad22[, c("oa11cd", "lsoa11cd")])

age_lookup <- fread("lookups/age_band_lookup.csv")

age_lookup_lsoa <- unique(age_lookup[, c("lsoa_deaths", "sya")])


## 4. creating the seed - splitting age from 5 year age bands into single year of age

  ### 4.1. small bit of cleaning up on the dataset
colnames(lsoa_deaths) <- tolower(colnames(lsoa_deaths))

lsoa_deaths_11 <- lsoa_deaths[, c("lsoa11cd", "gss_code", "year", "age_group", "sex", "deaths")]

lsoa_deaths_11_formar <- copy(lsoa_deaths_11)


### 4.2. splitting the 5-year age bands into single year of age
setkey(age_lookup_lsoa, "lsoa_deaths")
setkey(lsoa_deaths_11, "age_group")

lsoa_deaths_11 <- lsoa_deaths_11[age_lookup_lsoa, allow.cartesian = TRUE]

divs <- data.table(table(age_lookup_lsoa[, "lsoa_deaths"]))

setkey(divs, "lsoa_deaths")
setkey(lsoa_deaths_11, "age_group")

lsoa_deaths_11 <- divs[lsoa_deaths_11]

lsoa_deaths_11[, deaths := deaths/N]

lsoa_deaths_11 <- lsoa_deaths_11[, c("lsoa11cd", "gss_code", "year", "sya", "sex", "deaths")]
colnames(lsoa_deaths_11)[2] <- "lad22cd"


### 4.3. converting the seed table to lad23 geographies
### NOTE - it would be more efficient to convert the inputs to the correct geographies as soon as I read them in, instead of doing it several times bit by bit on the various outputs that come from that original input. Do this later. 

lsoa_seed <- recode_gss(df_in = lsoa_deaths_11, 
                             col_code = "lad22cd", 
                             col_data = "deaths", 
                             recode_from_year = 2020, 
                             recode_to_year = 2023)

lsoa_seed <- data.table(lsoa_seed)

## 5. creating the marginals

  ### 5.1. lad21 by year by sya by sex
lad_deaths <- lad_deaths[, c("gss_code", "year", "age", "sex", "value")]
colnames(lad_deaths)[1] <- "lad22cd"

lad_deaths[age %in% c(85, 86, 87, 88, 89, 90), age := 85]

lad_deaths <- lad_deaths[, .(value = sum(value)), 
                         by = list(lad22cd, year, age, sex)]

lad_deaths_mar <- lad_deaths

  ### 5.2. lsoa21 by lad21 by year by sex
lsoa_deaths_mar <- lsoa_deaths_11_formar[, .(deaths = sum(deaths)),
                                         by = list(lsoa11cd, gss_code, year, sex)]

colnames(lsoa_deaths_mar)[2] <- "lad22cd"

rm(lsoa_deaths_11_formar)
gc()


  ### 5.3. converting them both to lad23
lsoa_deaths_mar <- recode_gss(df_in = lsoa_deaths_mar, 
                              col_code = "lad22cd", 
                              col_data = "deaths", 
                              recode_from_year = 2020, 
                              recode_to_year = 2023)

lsoa_deaths_mar <- data.table(lsoa_deaths_mar)

lad_deaths_mar <- recode_gss(df_in = lad_deaths_mar, 
                             col_code = "lad22cd", 
                             col_data = "value",
                             recode_from_year = 2021,
                             recode_to_year = 2023)

lad_deaths_mar <- data.table(lad_deaths_mar)


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


## 6. saving the outputs
saveRDS(object = lsoa_seed,
        file = "input_data/intermediate/lsoa11_seed.rds")


saveRDS(object = lad_deaths_mar,
        file = "input_data/intermediate/lad_deaths_margin_lsoa11.rds") # pretty sure that this is the same dataset....but might as well save it separately, just in case, and then I can come back to it later. 


saveRDS(object = lsoa_deaths_mar,
        file = "input_data/intermediate/lsoa11_deaths_margin.rds")

rm(list = ls())
gc()
gc()
