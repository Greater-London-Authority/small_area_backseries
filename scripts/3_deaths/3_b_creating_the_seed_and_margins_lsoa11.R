## creates the inputs needed - the seed and the margin table - to feed into IPF to create single year of age death estimates on the lsoa11 boundaries
## again no need to make this general with respect to year. The lsoa11 deaths dataset was released up to 2023 and won't be updated again. So it's fine to hardcode the value of 2023 in there. 

## 0. libraries and functions
library(data.table)
library(gsscoder)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in the lsoa11 death data, narrowing to years
lsoa_deaths <- data.table(readRDS("input_data/intermediate/deathsbylsoa11midyear01to23.rds"))

lsoa_deaths <- lsoa_deaths[year >= 2010 & year <= 2023, ]


## 2. reading in the la level mid year estimates, extracting the death components

  ### 2.1. reading in, extracting death data
tmp <- tempfile(fileext = ".rds")
download.file(latest_gla_mye_url, tmp, mode = "wb")
mye_series <- data.table(readRDS(tmp))

lad_deaths <- mye_series[component == "deaths" & year >= 2010 & year <= 2023, 
                         c("gss_code", "gss_name", "year", "age", "sex", "value")]

lad_deaths[value < 0, value := 0]

rm(mye_series)
gc()

  ### 2.2. converting the lad geographies to 2023 
lad_deaths <- recode_gss(df_in = lad_deaths[, -"gss_name"], 
                         col_code = "gss_code", 
                         col_data = "value",
                         recode_from_year = 2021,
                         recode_to_year = 2023)

lad_deaths <- data.table(lad_deaths)


## 3. reading in the lookups
oa11_lsoa11_lad22 <- fread("lookups/2011_oa_lsoa_msoa_lad.csv")

lsoa11_lad22 <- unique(oa11_lsoa11_lad22[, c("lsoa11cd", "ladcd")])

colnames(lsoa11_lad22)[2] <- "lad22cd"

oa11_lsoa11 <- unique(oa11_lsoa11_lad22[, c("oa11cd", "lsoa11cd")])

age_lookup <- fread("lookups/age_band_lookup.csv")

age_lookup_lsoa <- unique(age_lookup[, c("lsoa_deaths", "sya")])


## 4. creating the seed - splitting age from 5 year age bands into single year of age

  ### 4.1. small bit of cleaning up on the dataset
colnames(lsoa_deaths) <- tolower(colnames(lsoa_deaths))

lsoa_deaths_11 <- lsoa_deaths[, c("lsoa11cd", "gss_code", "year", "age_group", "sex", "deaths")]

lsoa_deaths_11_formar <- copy(lsoa_deaths_11) # copying the dataset, because we'll need the original later to create one of the margins. formar means "for margin". 

  ### 4.2. splitting the 5-year age bands into single year of age (TO DO - need a function to do this)
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

lsoa_seed <- copy(lsoa_deaths_11)


## 5. creating the margins
## only two margins - fairly simple process at the moment, but fine for its intended purposes
## should put 5 year age bands in the margins, but given that the seed table is the 5-year split out into single, would make little difference

  ### 5.1. lad by year by sya by sex
lad_deaths <- lad_deaths[, c("gss_code", "year", "age", "sex", "value")]
colnames(lad_deaths)[1] <- "lad22cd"

lad_deaths_mar <- copy(lad_deaths)

  ### 5.2. lsoa11 by lad by year by sex
lsoa_deaths_mar <- lsoa_deaths_11_formar[, .(deaths = sum(deaths)),
                                         by = list(lsoa11cd, gss_code, year, sex)]

colnames(lsoa_deaths_mar)[2] <- "lad22cd"

rm(lsoa_deaths_11_formar)
gc()

  ### 5.4. scaling the lsoa figures so that the margins add up (IPF doesn't work if the margins give different totals)
  ### by getting the ratio difference between the two margins by year and sex and lad, and multiplying the lsoa values by these scaling factors
  ### this will lead to decimal points in the margin for lsoas. I think this is ok, because they're just an input into creating estimates for deaths, which we can round later if we like. 
  ### this also means that I am assuming that the local authority level mid year estimates are the "correct" estimates and that we scale the lsoa estimates to match them. This may or may not be a sound assumption, but it doesn't matter very much as the differences are very small. 
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

  ### 5.5. adding values of 0 for cells that are missing (in the dataset made available by ONS, it seems that they sometimes leave out a category entirely if there are no deaths, rather than leave it in with a value of 0. For example, if in a particular year in a particular lsoa, there were no deaths of 20 year olds, that combination of categories would be missing from the dataset rather than counted as 0). 
lsoa_seed_missing <- get_missing_categories(dataset = lsoa_seed, 
                                            cat_cols = c("lsoa11cd", "year", "sya", "sex"),
                                            value_col = "deaths")

lsoa_deaths_mar_missing <- get_missing_categories(dataset = lsoa_deaths_mar, 
                                                  cat_cols = c("year", "sex", "lsoa11cd"), 
                                                  value_col = "deaths") 

lsoa11_lad22 <- unique(lsoa_seed[, c("lsoa11cd", "lad22cd")])

setkey(lsoa11_lad22, "lsoa11cd")

setkey(lsoa_seed_missing, "lsoa11cd")
setkey(lsoa_deaths_mar_missing, "lsoa11cd")

lsoa_seed_missing <- lsoa11_lad22[lsoa_seed_missing]
lsoa_deaths_mar_missing <- lsoa11_lad22[lsoa_deaths_mar_missing]

lsoa_seed <- rbind(lsoa_seed, lsoa_seed_missing)
lsoa_deaths_mar <- rbind(lsoa_deaths_mar, lsoa_deaths_mar_missing)

lsoa_seed[is.na(deaths), deaths := 0]
lsoa_deaths_mar[is.na(deaths), deaths := 0]


## 6. saving the outputs
saveRDS(object = lsoa_seed,
        file = "input_data/intermediate/lsoa11_seed.rds")

saveRDS(object = lad_deaths_mar,
        file = "input_data/intermediate/lad_deaths_margin_lsoa11.rds")


saveRDS(object = lsoa_deaths_mar,
        file = "input_data/intermediate/lsoa11_deaths_margin.rds")


rm(list = ls())
gc()
gc()


