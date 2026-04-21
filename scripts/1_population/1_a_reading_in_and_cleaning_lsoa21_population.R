## note - could make most of this script into smaller functions

# 0. libraries and functions
library(nomisr)
library(data.table)
library(parallel)

source("scripts/inputs.R")


## 1. finding the right dataset on nomis, extracting the geography codes for the correct geography

  ### 1.1. getting the lsoa nomis id codes
lsoas_21_geogtab <- nomis_get_metadata(id = "NM_2014_1", # as a note, NM_2020_1 is the code for 2011-based. 
                                       concept = "geography",
                                       type = "TYPE151") # TYPE151 for 2021 lsoas

lsoas_21_geogvec <- lsoas_21_geogtab$id


## 2. creating the function to get a single lsoa of data
get_data_one_geog <- function(geog_id){
  
  extracted_geog_data <- nomis_get_data(id = "NM_2014_1",
                                        geography = geog_id,
                                        measures = 20100,
                                        select = c("date", "geography_name", "geography_code", "gender_name", "c_age_name", "c_age_type", "obs_value"))
  
  extracted_geog_data <- data.table(extracted_geog_data)
  
  return(extracted_geog_data)
  
}


## 3. extracting the data with parallel computing

  ### 3.1. setting up the cluster
no_cores <- round(detectCores()*0.75)

cl <- makeCluster(no_cores)

clusterEvalQ(cl = cl, expr = c(library(nomisr),
                               library(data.table)))

clusterExport(cl = cl, c("lsoas_21_geogvec", "get_data_one_geog"))

  ### 3.2. getting the data, binding into one data.table
lsoa_data <- parLapply(cl = cl,
                       X = lsoas_21_geogvec,
                       fun = get_data_one_geog)

lsoa_data <- rbindlist(lsoa_data)


## 4. filtering, selecting, renaming, etc
colnames(lsoa_data) <- tolower(colnames(lsoa_data))

lsoa_data <- lsoa_data[c_age_type == "Individual age" & gender_name != "Total" & c_age_name != "All Ages", ]


lsoa_data[, c_age_name := gsub("Age |Aged", "", c_age_name)]
lsoa_data[c_age_name == " 90+", c_age_name := 90]
lsoa_data[, c_age_name := as.numeric(c_age_name)]

lsoa_data[, gender_name := tolower(gender_name)]

lsoa_data <- lsoa_data[, c("date", "geography_name", "geography_code", "c_age_name", "gender_name", "obs_value")]

colnames(lsoa_data) <- c("year", "lsoa21nm", "lsoa21cd", "age", "sex", "population")

lsoa_data <- lsoa_data[year >= min_year & year <= max_year, ]


## 5. saving the dataset
file_path <- paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_lsoa21.rds")

saveRDS(object = lsoa_data,
        file_path)


rm(list = ls())

gc()
gc()