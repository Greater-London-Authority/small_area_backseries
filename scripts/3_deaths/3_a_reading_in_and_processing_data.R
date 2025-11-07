## standalone script to download and process deaths data from ons website
## right...they're actually at lsoa2011, not lsoa2021

## 0. libraries and functions
library(data.table)
library(openxlsx)

## 1. reading in the files

  ### 1.1. full 2010 to 2023 series (on lsoa11 boundaries)
url_10_23 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/14319deathsbylowerlayersuperoutputarealsoaenglandandwalesmidyearperiods1julyto30june2011to2020/deathsbylsoa1023final.zip"

tmp_zip <- tempfile(fileext = ".zip")
download.file(url_10_23, tmp_zip, mode = "wb")

unzip(tmp_zip, files = "deaths_by_LSOA_10-23_FINAL.xlsx", exdir = tempdir())

deaths_10_23_male <- read.xlsx(file.path(tempdir(), "deaths_by_LSOA_10-23_FINAL.xlsx"), 
                          sheet = 6,
                          startRow = 4)

deaths_10_23_male <- data.table(deaths_10_23_male)


deaths_10_23_female <- read.xlsx(file.path(tempdir(), "deaths_by_LSOA_10-23_FINAL.xlsx"), 
                               sheet = 7,
                               startRow = 4)

deaths_10_23_female <- data.table(deaths_10_23_female)

  ### 1.2. 2024 (on lsoa21 boundaries)
deaths_2024_male <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/3096deathsbylowerlayersuperoutputarealsoaenglandandwalesmidyear2023to2024/deathsbylsoamidyear24.xlsx",
                              sheet = 5,
                              startRow = 3)

deaths_2024_male <- data.table(deaths_2024_male)

deaths_2024_female <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/3096deathsbylowerlayersuperoutputarealsoaenglandandwalesmidyear2023to2024/deathsbylsoamidyear24.xlsx",
                              sheet = 6,
                              startRow = 3)

deaths_2024_female <- data.table(deaths_2024_female)


## 2. fixing up the 2010-2023 series
colnames(deaths_10_23_male) <- gsub("Males", "age", colnames(deaths_10_23_male))
colnames(deaths_10_23_female) <- gsub("Females", "age", colnames(deaths_10_23_female))

deaths_10_23_male[, sex := "male"]
deaths_10_23_female[, sex := "female"]

deaths_10_23 <- rbind(deaths_10_23_male, 
                      deaths_10_23_female)

colnames(deaths_10_23) <- gsub(".", "_", colnames(deaths_10_23), fixed = TRUE)
colnames(deaths_10_23) <- tolower(colnames(deaths_10_23))
colnames(deaths_10_23)[colnames(deaths_10_23) == "mid-year"] <- "year"


deaths_10_23 <- data.table::melt(deaths_10_23, id.vars = c("year", "local_authority_code", "local_authority_name", "lsoa_code", "lsoa_name", "sex"),
                     variable.name = "age_group", value.name = "deaths", variable.factor = FALSE)

deaths_10_23[, age_group := gsub("age_", "", age_group)]
deaths_10_23[, age_group := gsub("to_", "", age_group)]
deaths_10_23[age_group %in% c("01_04", "05_09"), age_group := gsub("0", "", age_group)]
deaths_10_23[age_group == "under_1", age_group := "0"]
deaths_10_23[age_group == "over_85", age_group := "85+"]

colnames(deaths_10_23) <- c("year", "gss_code", "la_name", "LSOA11CD", 
                            "LSOA11NM", "sex", "age_group", "deaths")


## 3. fixing up 2024 data
colnames(deaths_2024_male) <- gsub("Males", "age", colnames(deaths_2024_male))
colnames(deaths_2024_female) <- gsub("Females", "age", colnames(deaths_2024_female))

deaths_2024_male[, sex := "male"]
deaths_2024_female[, sex := "female"]

deaths_2024 <- rbind(deaths_2024_male, 
                      deaths_2024_female)

colnames(deaths_2024) <- gsub(".", "_", colnames(deaths_2024), fixed = TRUE)
colnames(deaths_2024) <- tolower(colnames(deaths_2024))
colnames(deaths_2024)[colnames(deaths_2024) == "mid-year"] <- "year"

deaths_2024[, age_over_85 := age_over_85 + age_over_90]
deaths_2024 <- deaths_2024[, -"age_over_90"]

deaths_2024 <- data.table::melt(deaths_2024, id.vars = c("year", "local_authority_code", "local_authority_name", "lsoa21_code", "lsoa21_name", "sex"),
                     variable.name = "age_group", value.name = "deaths", variable.factor = FALSE)

deaths_2024[, age_group := gsub("age_", "", age_group)]
deaths_2024[, age_group := gsub("to_", "", age_group)]
deaths_2024[age_group %in% c("01_04", "05_09"), age_group := gsub("0", "", age_group)]
deaths_2024[age_group == "under_1", age_group := "0"]
deaths_2024[age_group == "over_85", age_group := "85+"]

colnames(deaths_2024) <- c("year", "LSOA21CD", "LSOA21NM", # they mislabelled the columns - mixed up lsoa and lad. If they correct this in the raw dataset, this line and the next will need to be rewritten. 
                           "gss_code", "la_name", 
                           "sex", "age_group", "deaths")

deaths_2024 <- deaths_2024[, c("year", "gss_code", "la_name", "LSOA21CD", 
                               "LSOA21NM", "sex", "age_group", "deaths")]


## 4. manually fixing that very odd issue of the same lsoa being allocated to two different LAs in a very small number of cases, in the raw data released by ONS for lsoa11 boundaries
deaths_10_23[LSOA11CD == "E01008187", gss_code := "E08000037"]
deaths_10_23[LSOA11CD == "E01008187", la_name := "Gateshead"]

deaths_10_23[LSOA11CD == "E01023964", gss_code := "E07000241"]
deaths_10_23[LSOA11CD == "E01023964", la_name := "Welwyn Hatfield"]

deaths_10_23 <- deaths_10_23[, .(deaths = sum(deaths)),
                             by = list(year, gss_code, la_name, LSOA11CD, LSOA11NM, sex, age_group)]


## 4. saving the datasets
saveRDS(object = deaths_10_23,
        file = "input_data/intermediate/deathsbylsoa11midyear01to23.rds")

saveRDS(object = deaths_2024, 
        file = "input_data/intermediate/deathsbylsoa21midyear24.rds")

