
# 0. libraries and functions
library(nomisr)
library(data.table)
library(parallel)

source("scripts/inputs.R")


## 1. finding the right dataset on nomis, extracting the geography codes for the correct geography

  ### 1.1. getting the msoa id codes
msoas_21_geogtab <- nomis_get_metadata(id = "NM_2014_1",
                                       concept = "geography",
                                       type = "TYPE152") # type 152 is the code for msoa21

msoas_21_geogvec <- msoas_21_geogtab$id


## 2. creating the function to get a single msoa of data
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

clusterExport(cl = cl, c("msoas_21_geogvec", "get_data_one_geog"))

  ### 3.2. getting the data, binding into one data.table
msoa_data <- parLapply(cl = cl,
                       X = msoas_21_geogvec,
                       fun = get_data_one_geog)

msoa_data <- rbindlist(msoa_data)


## 4. filtering, selecting, renaming, etc
colnames(msoa_data) <- tolower(colnames(msoa_data))

msoa_data <- msoa_data[c_age_type == "Individual age" & gender_name != "Total" & c_age_name != "All Ages", ]


msoa_data[, c_age_name := gsub("Age |Aged", "", c_age_name)]
msoa_data[c_age_name == " 90+", c_age_name := 90]
msoa_data[, c_age_name := as.numeric(c_age_name)]

msoa_data[, gender_name := tolower(gender_name)]

msoa_data <- msoa_data[, c("date", "geography_name", "geography_code", "c_age_name", "gender_name", "obs_value")]

colnames(msoa_data) <- c("year", "msoa21nm", "msoa21cd", "age", "sex", "population")

msoa_data <- msoa_data[year >= min_year & year <= max_year, ]


## 5. saving the dataset
file_path <- paste0("input_data/intermediate/mid_year_rebased_", msoa_data[, min(year)], msoa_data[, max(year)], "_msoa21.rds")

saveRDS(object = msoa_data,
        file_path)

rm(list = ls())

gc()
gc()

