## reads in, from two of the ad hoc datasets made available on an ONS webpage, births at oa21 level from 2001 to 2024. 
## one file contains births from 2011 to 2021. The other contains births from 2022 onwards. 
## at the moment, have decided that it's not worth making this script general with respect to year. As long as it's an ad hoc release, there may be all sorts of formatting changes from year to year, and the quality assurance and standards re formatting etc will be looser. 
## so it should, I think, unfortunately be part of the workflow for processing ad hoc releases to manually have a look at the dataset and change the code as needed. 
## ideally though it still should run smoothly after changing the link for the 22_onwards dataset. 


## 0. libraries and functions
library(data.table)
library(openxlsx)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")


## 1. reading in data
births_01_21 <- read.xlsx(births_01_21_path, # they have mislabelled this file. It's to June 2022, not 2021. Removed 2022 from this - otherwise results will be off. 
                          sheet = 3,
                          startRow = 6) # for sex, 1 is male, 2 is female

births_01_21 <- data.table(births_01_21)

  ### NOTE on below: this workbook contains deaths too, at oa21 level. May be useful later. 
births_22_onwards <- read.xlsx(births_22_onwards_path,
                          sheet = 4,
                          startRow = 4)

births_22_onwards <- data.table(births_22_onwards)


## 2. fixing up the 2001 to 2021 data
births_01_21 <- melt(births_01_21, id.vars = c("Output.Area", "Sex")) # this pivots from wide to long

colnames(births_01_21) <- c("oa21cd", "sex", "year", "births")

births_01_21[, year := as.numeric(tstrsplit(year, "-")[[2]])] # coding year as the second year in the xxxx/yyyy mid-year format

births_01_21[, sex := as.character(sex)]

births_01_21[sex == 1, sex := "male"]
births_01_21[sex == 2, sex := "female"]

births_01_21 <- births_01_21[year != 2022, ] # need to remove this because they've included 2022 data in both datasets, and mislabelled the first. 


## 3. fixing up the 2022 to 2024 data
births_22_onwards <- melt(births_22_onwards, id.vars = c("Sex", "Output.Area"), variable.factor = FALSE)

colnames(births_22_onwards) <- c("sex", "oa21cd", "year", "births")

births_22_onwards <- births_22_onwards[, c("oa21cd", "sex", "year", "births")]

births_22_onwards[, year := as.numeric(year)]


## 4. combining the two datasets and saving the final result
births_01_onwards <- rbind(births_01_21,
                      births_22_onwards)

births_01_onwards[, year := as.numeric(year)]

saveRDS(object = births_01_onwards,
        file = paste0("input_data/intermediate/births_2001_", max_year, "_oa21.rds"))


## 5. below, doing everything for births on 2011 boundaries. Doing it in a quicker, less well documented way, because a lot of the steps will be the same and we only need this dataset for script 4b
## also, this is not something that will need to be updated as any stage

tmp_zip <- tempfile(fileext = ".zip")

download.file(births_11_boundaries_path, 
              tmp_zip, mode = "wb")

unzip(tmp_zip, files = "Mid year OA births FINAL.xlsx", exdir = tempdir())

births_oa11 <- read.xlsx(file.path(tempdir(), "Mid year OA births FINAL.xlsx"), 
                               sheet = 3,
                               startRow = 3)

births_oa11 <- data.table(births_oa11)

colnames(births_oa11)[1:2] <- c("oa11", "sex")

births_oa11 <- data.table::melt(births_oa11, id.vars = c("oa11", "sex"), variable.factor = FALSE)

births_oa11[, year := tstrsplit(variable, "-", fixed = TRUE)[2]]

births_oa11 <- births_oa11[, c("oa11", "sex", "year", "value")]

colnames(births_oa11) <- c("OA11CD", "sex", "year", "births") # set up in exactly the same way as the version we had saved locally that was used previously

births_oa11[sex == 1, sex := "male"]
births_oa11[sex == 2, sex := "female"]

saveRDS(object = births_oa11,
        file = "input_data/intermediate/births_oa11_mid_2002_2020.rds")

rm(list = ls())
gc()
gc()
