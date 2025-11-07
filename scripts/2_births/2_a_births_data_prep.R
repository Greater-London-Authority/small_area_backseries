
## 0. libraries and functions

library(data.table)
library(openxlsx)


functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in data
births_01_21 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/1949livebirthsbyoutputareaenglandandwalesjuly2001tojune2021/oa21birthsfinal.xlsx",
                          sheet = 3,
                          startRow = 6) # for sex, 1 is male, 2 is female

births_01_21 <- data.table(births_01_21)

  ### NOTE on below: that sheet contains deaths too, at oa level. But at oa21 level. May be useful later, but probably not. 
births_22_24 <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/2798livebirthsandnumberofdeathoccurencesby2021censusoutputareasandsexforenglandandwalesforperiodsmidyear2022to2024/birthsanddeathsmidyearfinal.xlsx",
                          sheet = 4,
                          startRow = 4)

births_22_24 <- data.table(births_22_24)


## 2. 2001 to 2021
births_01_21 <- melt(births_01_21, id.vars = c("Output.Area", "Sex"))

colnames(births_01_21) <- c("oa21cd", "sex", "year", "births")

births_01_21[, year := as.numeric(tstrsplit(year, "-")[[2]])]

births_01_21[, sex := as.character(sex)]

births_01_21[sex == 1, sex := "male"]
births_01_21[sex == 2, sex := "female"]


## 3. 2022 to 2024
births_22_24 <- melt(births_22_24, id.vars = c("Sex", "Output.Area"), variable.factor = FALSE)

colnames(births_22_24) <- c("sex", "oa21cd", "year", "births")

births_22_24 <- births_22_24[, c("oa21cd", "sex", "year", "births")]

births_22_24[, year := as.numeric(year)]

## 4. combining the two datasets and saving the final result
births_01_24 <- rbind(births_01_21,
                      births_22_24)

births_01_24[, year := as.numeric(year)]

saveRDS(object = births_01_24,
        file = "input_data/intermediate/births_2001_2024_oa21.rds")




