
# 0. libraries and functions
library(nomisr)
library(data.table)
library(parallel)

source("scripts/inputs.R")


## 1. finding the right dataset on nomis, extracting the geography codes for the correct geography

  ### 1.1. getting the ward id codes
wards_22_geogtab <- nomis_get_metadata(id = "NM_2014_1",
                                       concept = "geography",
                                       type = "TYPE153")

wards_22_geogvec <- wards_22_geogtab$id


## 2. creating the function to get a single ward of data
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

clusterExport(cl = cl, c("wards_22_geogvec", "get_data_one_geog"))

  ### 3.2. getting the data, binding into one data.table
ward_data <- parLapply(cl = cl,
                       X = wards_22_geogvec,
                       fun = get_data_one_geog)

ward_data <- rbindlist(ward_data)


## 4. filtering, selecting, renaming, etc
colnames(ward_data) <- tolower(colnames(ward_data))

ward_data <- ward_data[c_age_type == "Individual age" & gender_name != "Total" & c_age_name != "All Ages", ]


ward_data[, c_age_name := gsub("Age |Aged", "", c_age_name)]
ward_data[c_age_name == " 90+", c_age_name := 90]
ward_data[, c_age_name := as.numeric(c_age_name)]

ward_data[, gender_name := tolower(gender_name)]

ward_data <- ward_data[, c("date", "geography_name", "geography_code", "c_age_name", "gender_name", "obs_value")]

colnames(ward_data) <- c("year", "ward22nm", "ward22cd", "age", "sex", "population")

ward_data <- ward_data[year >= min_year & year <= max_year, ]


## 5. saving the dataset
file_path <- paste0("input_data/intermediate/mid_year_rebased_", min_year, max_year, "_ward22.rds")

saveRDS(object = ward_data,
        file_path)

rm(list = ls())

gc()
gc()

