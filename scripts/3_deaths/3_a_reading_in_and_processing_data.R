## standalone script to download and process deaths data from ons website
## have decided not to make this script general with respect to year. This is updated as an ad-hoc release, and often there are small changes to formatting or geographies that will need particular attention and manual changes. There are also sometimes errors with these releases... 
## as it is, the most recent 2024 dataset has an error in it, so no point in making it general while that error is still there. 

## 0. libraries and functions
library(data.table)
library(openxlsx)

source("scripts/inputs.R")


## 1. reading in the files

  ### 1.1. full 2010 to 2023 series (on lsoa11 boundaries)
  ### only available as a zip file, so need to download it as a temporary file and take out the specific excel sheet that we need
tmp_zip <- tempfile(fileext = ".zip")
download.file(deaths_url_10_23, tmp_zip, mode = "wb")

unzip(tmp_zip, files = "deaths_by_LSOA_10-23_FINAL.xlsx", exdir = tempdir())

deaths_10_23_male <- read.xlsx(file.path(tempdir(), "deaths_by_LSOA_10-23_FINAL.xlsx"), 
                          sheet = 6,
                          startRow = 4)

deaths_10_23_male <- data.table(deaths_10_23_male)


deaths_10_23_female <- read.xlsx(file.path(tempdir(), "deaths_by_LSOA_10-23_FINAL.xlsx"), 
                               sheet = 7,
                               startRow = 4)

deaths_10_23_female <- data.table(deaths_10_23_female)

  ### 1.2. 2024 onwards (on lsoa21 boundaries)
deaths_2024_male <- read.xlsx(deaths_24_onwards_path,
                              sheet = 5,
                              startRow = 3)

deaths_2024_male <- data.table(deaths_2024_male)

deaths_2024_female <- read.xlsx(deaths_24_onwards_path,
                              sheet = 6,
                              startRow = 3)

deaths_2024_female <- data.table(deaths_2024_female)


## 2. fixing up the 2010-2023 series

  ### 2.1. binding male and female datasets together and formatting
colnames(deaths_10_23_male) <- gsub("Males", "age", colnames(deaths_10_23_male))
colnames(deaths_10_23_female) <- gsub("Females", "age", colnames(deaths_10_23_female))

deaths_10_23_male[, sex := "male"]
deaths_10_23_female[, sex := "female"]

deaths_10_23 <- rbind(deaths_10_23_male, 
                      deaths_10_23_female)

colnames(deaths_10_23) <- gsub(".", "_", colnames(deaths_10_23), fixed = TRUE)
colnames(deaths_10_23) <- tolower(colnames(deaths_10_23))
colnames(deaths_10_23)[colnames(deaths_10_23) == "mid-year"] <- "year"

  ### 2.2. pivoting to long and renaming age categories
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

  ### 3.1. binding male and female datasets together and formatting
colnames(deaths_2024_male) <- gsub("Males", "age", colnames(deaths_2024_male))
colnames(deaths_2024_female) <- gsub("Females", "age", colnames(deaths_2024_female))

deaths_2024_male[, sex := "male"]
deaths_2024_female[, sex := "female"]

deaths_2024 <- rbind(deaths_2024_male, 
                      deaths_2024_female)

colnames(deaths_2024) <- gsub(".", "_", colnames(deaths_2024), fixed = TRUE)
colnames(deaths_2024) <- tolower(colnames(deaths_2024))
colnames(deaths_2024)[colnames(deaths_2024) == "mid-year"] <- "year"

deaths_2024[, age_over_85 := age_over_85 + age_over_90] # adding together 85+ and 90+, to make the full 85+ category. We will need to do everything with max cohort of over 90 anyway. But because it's a hassle that the two different deaths datasets have different max cohorts, and because I think the current process for "unrolling" the max cohort is pretty rough, and because this is just an input into IPF, I don't think it's worth the effort and am just keeping these aggregated here to be re-disaggregated into sya up to 90+ later. 
deaths_2024 <- deaths_2024[, -"age_over_90"]

  ### 3.2. pivoting to long, renaming age categories, and then fixing that mistake in the dataset of mislabelling lsoa and lad
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
                             by = list(year, gss_code, la_name, LSOA11CD, LSOA11NM, sex, age_group)] # column names probably shouldn't be capitalised, but the idea with this script was to process the data so that it would exactly fit with the format of the deaths data already used in other repos. 


## 4. saving the datasets
saveRDS(object = deaths_10_23,
        file = "input_data/intermediate/deathsbylsoa11midyear01to23.rds")

saveRDS(object = deaths_2024, 
        file = paste0("input_data/intermediate/deathsbylsoa21midyear_2024_to_", max_year, ".rds"))

rm(list = ls())
gc()


