
## min and max year. Min year, as the input population and component datasets are, is likely to remain 2011 for the time being as we don't have earlier data. Max year is the latest year that all input datasets are available on. 
max_year <- 2024
min_year <- 2011


## geography codes and any related objects
lad_version <- 2023 # the version of the lad boundaries that we want to recode to. Important only for the deaths ipf process at the moment. 


## links to external datasets
latest_gla_mye_url <- "https://data.london.gov.uk/download/ex9jd/ba752f34-0b54-4184-9251-8e2e94ae97ee/full_modelled_estimates_series_EW(2023_geog).rds" # mid-year estimates, local authority level

births_01_21_path <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/1949livebirthsbyoutputareaenglandandwalesjuly2001tojune2021/oa21birthsfinal.xlsx"
births_22_onwards_path <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/2798livebirthsandnumberofdeathoccurencesby2021censusoutputareasandsexforenglandandwalesforperiodsmidyear2022to2024/birthsanddeathsmidyearfinal.xlsx"
births_11_boundaries_path <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/13952livebirthsbyoutputareaenglandandwalesmidyearperiods1julyto30june2001to2020/midyearoabirthsfinal.zip"

deaths_url_10_23 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/14319deathsbylowerlayersuperoutputarealsoaenglandandwalesmidyearperiods1julyto30june2011to2020/deathsbylsoa1023final.zip"
deaths_24_onwards_path <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/3096deathsbylowerlayersuperoutputarealsoaenglandandwalesmidyear2023to2024/deathsbylsoamidyear24.xlsx" # assuming it will be 2024 onwards in one file with lsoa21 boundaries. It may be that they release subsequent years in separate files.

msoa11_population_path <- "https://data.london.gov.uk/download/ex9jd/grh/population_msoa11_2010_to_2011.csv"
lsoa11_population_path <- "https://data.london.gov.uk/download/ex9jd/feh/population_lsoa11_2010_to_2011.csv"


## paths to the lookups for aggregation
## for the process as it is currently written, we need lookups from lsoa11, oa21, and lsoa21. These all need to be matched with the same higher geography for the same run. 
## at the moment, the process assumes that the lookup from 2011 will be address weighted by year, and that the lookups from 2021 will be simple best fit. We may need to make it more flexible soon! 
lsoa11_lookup_path <- "lookups/lsoa11cd_to_mz.rds"

oa21_lookup_path <- "lookups/oa21cd_to_mz.rds"

lsoa21_lookup_path <- "lookups/lsoa21cd_to_mz.rds"

dest_geog_colname <- "mz_code" # all lookups need to have this as the column name of the destination geography
