
## 0. libraries and functions
library(data.table)
library(sf)


## 1. reading in geography files
lsoa_11_pwcs <- st_read("geo/LSOA_2011_EW_PWC.shp")

lsoa_21_pwcs <- st_read("geo/lsoa_21_pwcs.gpkg")

oa_11_pwcs <- st_read("geo/OA_2011_EW_PWC.shp")

oa_21_pwcs <- st_read("geo/Pop_Centroids_EnglandWales_2021.shp")

ward_22 <- st_read("geo/WD_DEC_2022_UK_BFC.shp")


## 2. oa21 to ward22
oa_21_pwcs <- st_transform(oa_21_pwcs, crs = 27700)
ward_22 <- st_transform(ward_22, crs = 27700)

lookup <- st_join(oa_21_pwcs, ward_22)
lookup <- data.table(lookup)

oa21_ward22 <- lookup[, c("oa21cd", "WD22CD", "WD22NM")]

colnames(oa21_ward22) <- c("oa21cd", "ward22cd", "ward22nm")


## 2. lsoa21 to ward22
lsoa_21_pwcs <- st_transform(lsoa_21_pwcs, crs = 27700)
ward_22 <- st_transform(ward_22, crs = 27700)

lookup <- st_join(lsoa_21_pwcs, ward_22)
lookup <- data.table(lookup)

lsoa21_ward22 <- lookup[, c("LSOA21CD", "WD22CD", "WD22NM")]

colnames(lsoa21_ward22) <- c("lsoa21cd", "ward22cd", "ward22nm")


## 3. lsoa11 to ward22

  ### 3.1. creating the non-weighted, E&W-wide lookup
lsoa_11_pwcs <- st_transform(lsoa_11_pwcs, crs = 27700)
ward_22 <- st_transform(ward_22, crs = 27700)

lookup <- st_join(lsoa_11_pwcs, ward_22)
lookup <- data.table(lookup)

lsoa11_ward22 <- lookup[, c("LSOA11CD", "WD22CD", "WD22NM")]

colnames(lsoa11_ward22) <- c("lsoa11cd", "ward22cd", "ward22nm")

  ### 3.2. merging the weighted and non-weighted lookups
lsoa11_ward22_london_weighted <- fread("lookups/LSOA_2011_London_Ward_2022_London_combined.csv")

colnames(lsoa11_ward22_london_weighted) <- tolower(colnames(lsoa11_ward22_london_weighted))
colnames(lsoa11_ward22_london_weighted)[colnames(lsoa11_ward22_london_weighted) == "gss_code"] <- "ward22cd"

lsoa11_ward22_london_weighted <- lsoa11_ward22_london_weighted[, c("lsoa11cd", "ward22cd", "year", "weight")]

london_lsoas <- lsoa11_ward22_london_weighted[, unique(lsoa11cd)]

lsoa11_ward22_lookup_nonlondon <- lsoa11_ward22[!(lsoa11cd %in% london_lsoas), ]

lsoa11_ward22_lookup_nonlondon[, year := 2011]

years <- 2011:2021

lsoa11_ward22_lookup_nonlondon <- lsoa11_ward22_lookup_nonlondon[, .(year = years),
                                                             by = eval(names(lsoa11_ward22_lookup_nonlondon)[names(lsoa11_ward22_lookup_nonlondon) != "year"])]

lsoa11_ward22_lookup_nonlondon[, weight := 1]
lsoa11_ward22_lookup_nonlondon <- lsoa11_ward22_lookup_nonlondon[, c("lsoa11cd", "ward22cd", "year", "weight")]

lsoa11_ward22_all <- rbind(lsoa11_ward22_london_weighted, lsoa11_ward22_lookup_nonlondon)

lsoa11_ward22_all_2021 <- lsoa11_ward22_all[year == 2021, ]

lsoa11_ward22_all_2022 <- copy(lsoa11_ward22_all_2021)
lsoa11_ward22_all_2022[, year := 2022]

lsoa11_ward22_all_2023 <- copy(lsoa11_ward22_all_2021)
lsoa11_ward22_all_2023[, year := 2023]

lsoa11_ward22_all_2024 <- copy(lsoa11_ward22_all_2021)
lsoa11_ward22_all_2024[, year := 2024]

lsoa11_ward22_all_full <- rbind(lsoa11_ward22_all, 
                                lsoa11_ward22_all_2022,
                                lsoa11_ward22_all_2023,
                                lsoa11_ward22_all_2024)


## 4. saving the lookups
saveRDS(object = oa21_ward22,
        file = "lookups/oa21_ward22_bf.rds")

saveRDS(object = lsoa21_ward22,
        file = "lookups/lsoa21_ward22_bf.rds")

saveRDS(object = lsoa11_ward22,
        file = "lookups/lsoa11_ward22_bf.rds")

saveRDS(object = lsoa11_ward22_all_full,
        file = "lookups/lsoa11_ward22_weighted_extended.rds")


