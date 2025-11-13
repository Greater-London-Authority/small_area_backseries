
## 0. libraries and functions
library(data.table)


## 1. reading in lookups
oa11_msoa21_weighted <- readRDS("lookups/oa11_msoa21_weighted.rds")

lsoa11_msoa21_weighted <- readRDS("lookups/lsoa11_msoa21_weighted.rds")

oa11_ward22_london_weighted <- fread("lookups/OA_2011_London_Ward_2022_London_combined_update.csv")

lsoa11_ward22_london_weighted <- fread("lookups/LSOA_2011_London_Ward_2022_London_combined.csv")

oa11_ward22_lookup <- fread()
lsoa11_ward22_lookup <- fread()


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


## 4. adding on the non_london geographies to the weighted lookups

  ### 4.1. oa-ward22
colnames(oa11_ward22_london_weighted) <- tolower(colnames(oa11_ward22_london_weighted))
colnames(oa11_ward22_london_weighted)[colnames(oa11_ward22_london_weighted) == "gss_code"] <- "ward22cd"

oa11_ward22_london_weighted <- oa11_ward22_london_weighted[, c("oa11cd", "ward22cd", "year", "weight")]

london_oas <- oa11_ward22_london_weighted[, unique(oa11cd)]

oa11_ward22_lookup_nonlondon <- oa11_ward22_lookup[!(oa11cd %in% london_oas), ]

oa11_ward22_lookup_nonlondon[, year := 2011]

years <- 2011:2021

oa11_ward22_lookup_nonlondon <- oa11_ward22_lookup_nonlondon[, .(year = years),
                                                             by = eval(names(oa11_ward22_lookup_nonlondon)[names(oa11_ward22_lookup_nonlondon) != "year"])]

oa11_ward22_lookup_nonlondon[, weight := 1]

oa11_ward22_all <- rbind(oa11_ward22_london_weighted, oa11_ward22_lookup_nonlondon)

saveRDS(object = oa11_ward22_all,
        file = "lookups/oa11_ward22_weighted.rds")

  ### 4.2. oa-msoa21cd
colnames(oa11_msoa21_london_weighted) <- tolower(colnames(oa11_msoa21_london_weighted))
oa11_msoa21_london_weighted <- oa11_msoa21_london_weighted[, c("oa11cd", "msoa21cd", "year", "weight")]

london_oas <- oa11_msoa21_london_weighted[, unique(oa11cd)]

oa11_msoa21_lookup_nonlondon <- oa11_msoa21_lookup[!(oa11cd %in% london_oas), ]

oa11_msoa21_lookup_nonlondon[, year := 2011]

years <- 2011:2021

oa11_msoa21_lookup_nonlondon <- oa11_msoa21_lookup_nonlondon[, .(year = years),
                                                             by = eval(names(oa11_msoa21_lookup_nonlondon)[names(oa11_msoa21_lookup_nonlondon) != "year"])]

oa11_msoa21_lookup_nonlondon[, weight := 1]

oa11_msoa21_all <- rbind(oa11_msoa21_london_weighted, oa11_msoa21_lookup_nonlondon)

saveRDS(object = oa11_msoa21_all,
        file = "lookups/oa11_msoa21_weighted.rds")


## 5. saving the results
saveRDS(object = oa11_msoa21_weighted_full, 
        file = "lookups/oa11_msoa21_weighted_extended.rds")

saveRDS(object = lsoa11_msoa21_weighted_full, 
        file = "lookups/lsoa11_msoa21_weighted_extended.rds")

rm(list = ls())
gc()
