

## 1. libraries and functions
library(data.table)
library(openxlsx)
library(gsscoder)


## 2.  reading in the raw lsoa-level rebased mid-year estimates from 2011 to 2022
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

mye_2022 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019tomid2022/sapelsoasyoa20192022.xlsx",
                      sheet = 8,
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

  ### 3.3. final getting them all into one
mye_11_22 <- rbind(mye_2011, mye_2012, mye_2013, mye_2014, mye_2015, mye_2016, mye_2017, mye_2018, mye_2019, mye_2020, mye_2021, mye_2022)


## 4. pivoting longer and adding columns for age and sex
mye_11_22 <- data.table::melt(mye_11_22, id.vars = c("LAD.2021.Code", "LAD.2021.Name", "LSOA.2021.Code", "LSOA.2021.Name", "year"))

mye_11_22 <- mye_11_22[variable != "Total", ]

mye_11_22 <- mye_11_22[, sex := "empty"]

mye_11_22[grepl("F", variable), sex := "female"]
mye_11_22[grepl("M", variable), sex := "male"]

mye_11_22[, age := as.numeric(gsub("M|F", "", variable))]


## 5. final cleaning and reading out
colnames(mye_11_22) <- c("lad21cd", "lad21nm", "lsoa21cd", "lsoa21nm", "year", "variable", "population", "sex", "age")

mye_11_22 <- mye_11_22[, c("lad21cd", "lad21nm", "lsoa21cd", "lsoa21nm", "year", "age", "sex", "population")]

fwrite(x = mye_11_22,
      file = "input_data/intermediate/mid_year_rebased_20112022_lsoa21.csv")

rm(list = ls())
gc()

