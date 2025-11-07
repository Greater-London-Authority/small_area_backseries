## 

## 0. libraries and functions
library(data.table)


## 1. reading in lookups
oa11_msoa21_weighted <- readRDS("lookups/oa11_msoa21_weighted.rds")

lsoa11_msoa21_weighted <- readRDS("lookups/lsoa11_msoa21_weighted.rds")


## 2. adding on years to the oa lookup
oa11_msoa21_weighted_2021 <- oa11_msoa21_weighted[year == 2021, ]

oa11_msoa21_weighted_2022 <- copy(oa11_msoa21_weighted_2021)
oa11_msoa21_weighted_2022[, year := 2022]

oa11_msoa21_weighted_2023 <- copy(oa11_msoa21_weighted_2021)
oa11_msoa21_weighted_2023[, year := 2023]

oa11_msoa21_weighted_2024 <- copy(oa11_msoa21_weighted_2021)
oa11_msoa21_weighted_2024[, year := 2024]

oa11_msoa21_weighted_full <- rbind(oa11_msoa21_weighted, 
                                   oa11_msoa21_weighted_2022,
                                   oa11_msoa21_weighted_2023,
                                   oa11_msoa21_weighted_2024)



## 3. adding on years to the lsoa lookup
lsoa11_msoa21_weighted_2021 <- lsoa11_msoa21_weighted[year == 2021, ]

lsoa11_msoa21_weighted_2022 <- copy(lsoa11_msoa21_weighted_2021)
lsoa11_msoa21_weighted_2022[, year := 2022]

lsoa11_msoa21_weighted_2023 <- copy(lsoa11_msoa21_weighted_2021)
lsoa11_msoa21_weighted_2023[, year := 2023]

lsoa11_msoa21_weighted_2024 <- copy(lsoa11_msoa21_weighted_2021)
lsoa11_msoa21_weighted_2024[, year := 2024]

lsoa11_msoa21_weighted_full <- rbind(lsoa11_msoa21_weighted, 
                                   lsoa11_msoa21_weighted_2022,
                                   lsoa11_msoa21_weighted_2023,
                                   lsoa11_msoa21_weighted_2024)



## 4. saving the results
saveRDS(object = oa11_msoa21_weighted_full, 
        file = "lookups/oa11_msoa21_weighted_extended.rds")

saveRDS(object = lsoa11_msoa21_weighted_full, 
        file = "lookups/lsoa11_msoa21_weighted_extended.rds")

rm(list = ls())
gc()
