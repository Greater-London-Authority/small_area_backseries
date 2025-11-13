

## 0. libraries and functions
library(sf)
library(data.table)

## 1. creating 2011 output to 2021 census statistical geographies (above OA) and to 2022 local authority districts (ended up not using a lot of these. Need to be weighted)

oa_11_pwcs <- st_read("geo/OA_2011_EW_PWC.shp")

lsoa_21 <- st_read("geo/LSOA_2021_EnglandWales_full_extent.shp")

oa_11_pwcs <- st_transform(oa_11_pwcs, crs = 27700)
lsoa_21 <- st_transform(lsoa_21, crs = 27700)

lookup <- st_join(oa_11_pwcs, lsoa_21)
lookup <- data.table(lookup)

colnames(lookup)[1] <- "oa11cd"

keep <- c("oa11cd","lsoa21cd","lsoa21nm",
          "msoa21cd","msoa21nm","lad22cd","lad22nm")

lookup <- lookup[,..keep]

fwrite(x = lookup,
       file = "lookups/2011_oa_2022_lsoa_msoa_la.csv")


## 2. oa11 to msoa21
msoa_21 <- st_read("geo/MSOA_2021_EnglandWales_full_extent.shp")

oa_11_pwcs <- st_transform(oa_11_pwcs, crs = 27700)
msoa_21 <- st_transform(msoa_21, crs = 27700)

oa11_msoa21_lookup <- st_join(oa_11_pwcs, msoa_21)
oa11_msoa21_lookup <- data.table(oa11_msoa21_lookup)


colnames(oa11_msoa21_lookup) <- tolower(colnames(oa11_msoa21_lookup))

oa11_msoa21_lookup <- oa11_msoa21_lookup[, c("oa11cd", "msoa21cd")]

oa11_msoa21_london_weighted <- fread("lookups/OA_2011_London_MSOA_2021_London_combine.csv")

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


## 3. lsoa11 to msoa21
lsoa_11_pwcs <- st_read("geo/LSOA_2011_EW_PWC.shp")

lsoa_11_pwcs <- st_transform(lsoa_11_pwcs, crs = 27700)
msoa_21 <- st_transform(msoa_21, crs = 27700)

lsoa11_msoa21_lookup <- st_join(lsoa_11_pwcs, msoa_21)
lsoa11_msoa21_lookup <- data.table(lsoa11_msoa21_lookup)


colnames(lsoa11_msoa21_lookup) <- tolower(colnames(lsoa11_msoa21_lookup))

lsoa11_msoa21_lookup <- lsoa11_msoa21_lookup[, c("lsoa11cd", "msoa21cd")]

lsoa11_msoa21_london_weighted <- fread("lookups/LSOA_2011_London_MSOA_2021_London_combined.csv")

colnames(lsoa11_msoa21_london_weighted) <- tolower(colnames(lsoa11_msoa21_london_weighted))
lsoa11_msoa21_london_weighted <- lsoa11_msoa21_london_weighted[, c("lsoa11cd", "msoa21cd", "year", "weight")]

london_lsoas <- lsoa11_msoa21_london_weighted[, unique(lsoa11cd)]

lsoa11_msoa21_lookup_nonlondon <- lsoa11_msoa21_lookup[!(lsoa11cd %in% london_lsoas), ]

lsoa11_msoa21_lookup_nonlondon[, year := 2011]

years <- 2011:2021

lsoa11_msoa21_lookup_nonlondon <- lsoa11_msoa21_lookup_nonlondon[, .(year = years),
                                                             by = eval(names(lsoa11_msoa21_lookup_nonlondon)[names(lsoa11_msoa21_lookup_nonlondon) != "year"])]

lsoa11_msoa21_lookup_nonlondon[, weight := 1]

lsoa11_msoa21_all <- rbind(lsoa11_msoa21_london_weighted, lsoa11_msoa21_lookup_nonlondon)

saveRDS(object = lsoa11_msoa21_all,
        file = "lookups/lsoa11_msoa21_weighted.rds")



## 4. 2021 lsoa to 2011 msoa

lsoa_21_centroids <- st_read("geo/lsoa_21_pwcs.gpkg")

msoa_11 <- st_read("geo/MSOA_2011_EW.shp")

lsoa_21_centroids <- st_transform(lsoa_21_centroids, crs = 27700)
msoa_11 <- st_transform(msoa_11, crs = 27700)


lsoa21_msoa11_lookup <- st_join(lsoa_21_centroids, msoa_11)

lsoa21_msoa11_lookup <- data.table(lsoa21_msoa11_lookup)

lsoa21_msoa11_lookup <- lsoa21_msoa11_lookup[, c("MSOA11CD", "LSOA21CD")]

colnames(lsoa21_msoa11_lookup) <- c("msoa11cd", "lsoa21cd")

fwrite(x = lsoa21_msoa11_lookup,
      file = "lookups/2021_lsoa_2011_msoa.csv")

