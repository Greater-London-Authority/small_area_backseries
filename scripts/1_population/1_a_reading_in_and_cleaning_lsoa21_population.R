

## 1. libraries and functions
library(data.table)
library(openxlsx)
library(gsscoder)

source("scripts/inputs.R")

## 2.  reading in the raw lsoa-level rebased mid-year estimates from 2011 to the max year
mye_2011 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2011tomid2014/sapelsoasyoa20112014.xlsx",
                      sheet = 5,
                      startRow = 4)

mye_2012 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2011tomid2014/sapelsoasyoa20112014.xlsx",
                      sheet = 6,
                      startRow = 4)

mye_2013 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2011tomid2014/sapelsoasyoa20112014.xlsx",
                      sheet = 7,
                      startRow = 4)

mye_2014 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2011tomid2014/sapelsoasyoa20112014.xlsx",
                      sheet = 8,
                      startRow = 4)

mye_2015 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015tomid2018/sapelsoasyoa20152018.xlsx",
                      sheet = 5,
                      startRow = 4)

mye_2016 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015tomid2018/sapelsoasyoa20152018.xlsx",
                      sheet = 6,
                      startRow = 4)

mye_2017 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015tomid2018/sapelsoasyoa20152018.xlsx",
                      sheet = 7,
                      startRow = 4)

mye_2018 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015tomid2018/sapelsoasyoa20152018.xlsx",
                      sheet = 8,
                      startRow = 4)

mye_2019 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019tomid2022/sapelsoasyoa20192022.xlsx",
                      sheet = 5,
                      startRow = 4)

mye_2020 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019tomid2022/sapelsoasyoa20192022.xlsx",
                      sheet = 6,
                      startRow = 4)

mye_2021 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019tomid2022/sapelsoasyoa20192022.xlsx",
                      sheet = 7,
                      startRow = 4)

mye_2022 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
                      sheet = 5,
                      startRow = 4)

mye_2023 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
                      sheet = 6,
                      startRow = 4)

mye_2024 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
                      sheet = 7,
                      startRow = 4)


## 3. getting them all into one dataset

  ### 3.1. converting them all to data.table (think it's quickest to do this manually)
mye_2011 <- data.table(mye_2011)
mye_2012 <- data.table(mye_2012)
mye_2013 <- data.table(mye_2013)
mye_2014 <- data.table(mye_2014)
mye_2015 <- data.table(mye_2015)
mye_2016 <- data.table(mye_2016)
mye_2017 <- data.table(mye_2017)
mye_2018 <- data.table(mye_2018)
mye_2019 <- data.table(mye_2019)
mye_2020 <- data.table(mye_2020)
mye_2021 <- data.table(mye_2021)
mye_2022 <- data.table(mye_2022)
mye_2023 <- data.table(mye_2023)
mye_2024 <- data.table(mye_2024)

  ### 3.2. adding year (again, I think it's quickest to do this manually, because I'm looping through data.tables. Another option would have been to put them all in a list, and then use lapply. Possibly a better option.)
mye_2011[, year := 2011]
mye_2012[, year := 2012]
mye_2013[, year := 2013]
mye_2014[, year := 2014]
mye_2015[, year := 2015]
mye_2016[, year := 2016]
mye_2017[, year := 2017]
mye_2018[, year := 2018]
mye_2019[, year := 2019]
mye_2020[, year := 2020]
mye_2021[, year := 2021]
mye_2022[, year := 2022]
mye_2023[, year := 2023]
mye_2024[, year := 2024]

  ### 3.3. final getting them all into one
colnames(mye_2022) <- gsub("2023", "2021", colnames(mye_2022)) # coding everything back to 2021, because that's currently what the rest of the repo is set up for. Although it might be good to have a process at the start of every script to update every data set with lad codes to the most recent. 
colnames(mye_2023) <- gsub("2023", "2021", colnames(mye_2023))
colnames(mye_2024) <- gsub("2023", "2021", colnames(mye_2024))

mye_11_maxyear <- rbind(mye_2011, mye_2012, mye_2013, mye_2014, mye_2015, mye_2016, mye_2017, mye_2018, mye_2019, mye_2020, mye_2021, mye_2022, mye_2023, mye_2024)


## 4. pivoting longer, adding columns for age and sex, fixing up the codes
mye_11_maxyear <- data.table::melt(mye_11_maxyear, id.vars = c("LAD.2021.Code", "LAD.2021.Name", "LSOA.2021.Code", "LSOA.2021.Name", "year"))

mye_11_maxyear <- mye_11_maxyear[variable != "Total", ]

mye_11_maxyear <- mye_11_maxyear[, sex := "empty"]

mye_11_maxyear[grepl("F", variable), sex := "female"]
mye_11_maxyear[grepl("M", variable), sex := "male"]

mye_11_maxyear[, age := as.numeric(gsub("M|F", "", variable))]


  ### 4.1. recoding 2022-2024 back to lad21
mye_22_24 <- mye_11_maxyear[year >= 2022 & year <= 2024, ]

mye_22_24 <- gsscoder::recode_gss(df_in = mye_22_24[, -"LAD.2021.Name"], 
                                  col_code = "LAD.2021.Code",
                                  col_data = "value",
                                  recode_from_year = 2023,
                                  recode_to_year = 2021)

mye_22_24 <- data.table(mye_22_24)

la_names_lookup <- unique(mye_2021[, c("LAD.2021.Code", "LAD.2021.Name")])

mye_22_24 <- la_names_lookup[mye_22_24, on = "LAD.2021.Code"]

mye_11_maxyear <- mye_11_maxyear[year < 2022, ]

mye_11_maxyear <- rbind(mye_11_maxyear,
                        mye_22_24)


## 5. final cleaning and reading out
colnames(mye_11_maxyear) <- c("lad21cd", "lad21nm", "lsoa21cd", "lsoa21nm", "year", "variable", "population", "sex", "age")

mye_11_maxyear <- mye_11_maxyear[, c("lad21cd", "lad21nm", "lsoa21cd", "lsoa21nm", "year", "age", "sex", "population")]

file_path <- paste0("input_data/intermediate/mid_year_rebased_2011", max_year, "_lsoa21.rds")

saveRDS(object = mye_11_maxyear, 
        file = file_path) 

rm(list = ls())
gc()
