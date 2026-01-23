
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

## allocate E01027798 to ward E05014328, and allocate W01000060 to ward W05001515 
## fixing the problem by allocating one of the lsoas to a ward at random that is in the same la. Very imperfect...but doesn't matter so much.

lsoa11_ward22[lsoa11cd == "E01027798", ward22cd := "E05014328"]
lsoa11_ward22[lsoa11cd == "E01027798", ward22nm := "Thornton Dale & Wolds"]

lsoa11_ward22[lsoa11cd == "W01000060", ward22cd := "W05001515"]
lsoa11_ward22[lsoa11cd == "W01000060", ward22nm := "Bro Dysynni"]


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


## 4. getting rid of multiple ward-la connections in the weighted lsoa11 to ward22 lookup
lsoa11_ward22_all_full <- lsoa11_ward22_all_full[weight != 0, ] # getting rid of those with a weight of 0 


  # CHECKING ERROR
#lsoa11_lad_lookup <- fread("lookups/2011_oa_lsoa_msoa_lad.csv") # checking if this has sorted the problem
#lsoa11_lad_lookup <- unique(lsoa11_lad_lookup[, c("lsoa11cd", "ladcd")])

#setkey(lsoa11_ward22_all_full, "lsoa11cd")
#setkey(lsoa11_lad_lookup, "lsoa11cd")


#nrow(lsoa11_ward22_all_full)
#lsoa11_ward22_all_full <- lsoa11_lad_lookup[lsoa11_ward22_all_full]

#ward_lad_relationships <- unique(lsoa11_ward22_all_full[, c("ward22cd", "ladcd")])

#ward_counts <- ward_lad_relationships[, .(count = length(ladcd)),
#                       by = list(ward22cd)]

#ward_counts[count != 1, ]
## two remaining wards that are allocated to multiple lads. E05014284 and W05001131. Just change them manually. Also no need to overthink it - they're outside of London. 

#lsoa11_ward22_all_full[ward22cd == "E05014284", ] # agh...they all have a weight of 1. Right, so these particular ones are introduced by the best fit lookup. 
## E01027798 is in E07000167, E01027833 and E01027834 and E01027835 are in E07000168


#lsoa11_ward22_all_full[ward22cd == "W05001131", ]
## W01000060 is in W06000002, W01000442 is in W06000023

  # END OF CHECKING ERROR

## NOTE for when I come back. I have fixed this best fit lookup that I've created, but what about the others? Need to check....because it could throw off the results

## converting city wards to city overall
city_wards <- c("E05009288", "E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296", "E05009297", "E05009298", "E05009299", 
                "E05009300", "E05009301", "E05009302", "E05009303", "E05009304", "E05009305", "E05009306", "E05009307", "E05009308", "E05009309", "E05009310", "E05009311", "E05009312")

oa21_ward22[ward22cd %in% city_wards, ward22nm := "City of London"]
oa21_ward22[ward22cd %in% city_wards, ward22cd := "E09000001"]

lsoa21_ward22[ward22cd %in% city_wards, ward22nm := "City of London"]
lsoa21_ward22[ward22cd %in% city_wards, ward22cd := "E09000001"]

lsoa11_ward22[ward22cd %in% city_wards, ward22nm := "City of London"]
lsoa11_ward22[ward22cd %in% city_wards, ward22cd := "E09000001"]

lsoa11_ward22_all_full[ward22cd %in% city_wards, ward22cd := "E09000001"]


## adjusting the lsoas lookups for those that are unmatcheable, also extracting the wards that haven't matched

  ### getting the full definitive list of 2022 wards
ward_def <- fread("lookups/Ward_to_Local_Authority_District_(December_2022)_Lookup_in_the_United_Kingdom.csv")
colnames(ward_def) <- tolower(colnames(ward_def))
ward_def <- ward_def[grepl("E|W", wd22cd), ]

city_wards <- c("E05009288", "E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296", "E05009297", "E05009298", "E05009299", 
                "E05009300", "E05009301", "E05009302", "E05009303", "E05009304", "E05009305", "E05009306", "E05009307", "E05009308", "E05009309", "E05009310", "E05009311", "E05009312")

ward_def[wd22cd %in% city_wards, wd22nm := "City of London"]
ward_def[wd22cd %in% city_wards, wd22cd := "E09000001"]

unique_wards_def <- ward_def[, unique(wd22cd)]

  ### getting the list of wards that are missing in either lookup
missing_wards_lsoa21 <- setdiff(unique_wards_def, lsoa21_ward22[, unique(ward22cd)])
missing_wards_lsoa11 <- setdiff(unique_wards_def, lsoa11_ward22_all_full[, unique(ward22cd)])

all_missing_wards <- unique(c(missing_wards_lsoa11, missing_wards_lsoa21))


  ### allocating lsoas to unmatched
lsoa21_ward22[ward22cd %in% all_missing_wards, ward22nm := "unmatched"]
lsoa21_ward22[ward22cd %in% all_missing_wards, ward22cd := "unmatched"]

lsoa21_ward22[is.na(ward22cd), ward22cd := "unmatched"]
lsoa21_ward22[is.na(ward22nm), ward22nm := "unmatched"]

lsoa21_ward22 <- unique(lsoa21_ward22)


lsoa11_ward22_all_full[ward22cd %in% all_missing_wards, ward22cd := "unmatched"]
lsoa11_ward22_all_full[is.na(ward22cd), ward22cd := "unmatched"]

lsoa11_ward22_all_full <- unique(lsoa11_ward22_all_full)


oa21_ward22[is.na(ward22cd), ward22cd := "unmatched"]
oa21_ward22[is.na(ward22nm), ward22nm := "unmatched"]

oa21_ward22[ward22cd%in% all_missing_wards, ward22nm := "unmatched"]
oa21_ward22[ward22cd%in% all_missing_wards, ward22cd := "unmatched"]

oa21_ward22 <- unique(oa21_ward22)

## 5. saving the lookups
saveRDS(object = oa21_ward22,
        file = "lookups/oa21_ward22_bf.rds")

saveRDS(object = lsoa21_ward22,
        file = "lookups/lsoa21_ward22_bf.rds")

saveRDS(object = lsoa11_ward22,
        file = "lookups/lsoa11_ward22_bf.rds")

saveRDS(object = lsoa11_ward22_all_full,
        file = "lookups/lsoa11_ward22_weighted_extended.rds")

saveRDS(object = all_missing_wards,
        file = "lookups/ward22_missing.rds")
