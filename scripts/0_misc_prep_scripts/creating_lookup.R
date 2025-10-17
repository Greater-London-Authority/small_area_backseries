

## 0. libraries and functions
library(sf)
library(data.table)

## 1. creating 2011 output to 2021 census statistical geographies (above OA) and to 2022 local authority districts

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


## 2021 lsoa to 2011 msoa

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


