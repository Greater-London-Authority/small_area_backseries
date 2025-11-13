
## 0. libraries and functions
library(data.table)
library(mipfp)
library(reshape2)
library(foreach)
library(doParallel)


## 1. reading in the data - margins and seed table completed in the previous script - and quick cleaning up

### 1.1. reading in data
lsoa_seed <- readRDS("input_data/intermediate/lsoa21_seed.rds")

lad_deaths_mar <- readRDS("input_data/intermediate/lad_deaths_margin_lsoa21.rds")

lsoa_deaths_mar <- readRDS("input_data/intermediate/lsoa21_deaths_margin.rds")

### 1.2. rearranging the columns - makes it slightly easier to line up the seed and marginal dimensions
lsoa_seed <- lsoa_seed[, c("lad22cd", "lsoa21cd", "year", "sya", "sex", "deaths")]

lsoa_deaths_mar <- lsoa_deaths_mar[, c("lad22cd", "lsoa21cd", "year", "sex", "deaths")]

lad_deaths_mar <- lad_deaths_mar[, c("lad22cd", "year", "age", "sex", "value")]


## 2. defining the function that will carry out the ipf
all_geogs <- lsoa_seed[, unique(lad22cd)]
input_geog <- all_geogs[1]

ipf_est <- function(input_geog){
  
  ### 2.1. narrowing down seed and marginals by geography
  lsoa_seed_sel <- lsoa_seed[lad22cd == input_geog, ]
  
  lsoa_deaths_mar_sel <- lsoa_deaths_mar[lad22cd == input_geog, ]
  
  lad_deaths_mar_sel <- lad_deaths_mar[lad22cd == input_geog, ]
  
  ### 2.2. converting the marginals into the form required by the mipfp package, a multidimensional array
  lsoa_year_sex_mar_ini <- tapply(X = lsoa_deaths_mar_sel$deaths,
                                  INDEX = list(lsoa_deaths_mar_sel$lsoa21cd,
                                               lsoa_deaths_mar_sel$year,
                                               lsoa_deaths_mar_sel$sex),
                                  FUN = sum)
  
  year_age_sex_mar_ini <- tapply(X = lad_deaths_mar_sel$value,
                                 INDEX = list(lad_deaths_mar_sel$year,
                                              lad_deaths_mar_sel$age,
                                              lad_deaths_mar_sel$sex),
                                 FUN = sum)
  
  
  ### 2.3. converting the seed table into the form required by the mipfp package, a multidimensional array
  lsoa_seed_sel_ar <- tapply(X = lsoa_seed_sel$deaths,
                             INDEX = list(lsoa_seed_sel$lsoa21cd,
                                          lsoa_seed_sel$year,
                                          lsoa_seed_sel$sya,
                                          lsoa_seed_sel$sex),
                             FUN = sum)
  
  
  ### 2.4. setting up the dimensions  
  tgt_data <- list(lsoa_year_sex_mar_ini,
                   year_age_sex_mar_ini)
  
  tgt_list <- list(c(1, 2, 4),
                   c(2, 3, 4))
  
  ### 2.5. runnnig the IPF algorithm
  res <- (Estimate(seed = lsoa_seed_sel_ar,
                   target.list = tgt_list,
                   target.data = tgt_data,
                   method = "ipfp",
                   tol = 1e-8, # tolerance was slightly arbitrarily chosen. Put some effort into a good rationale for the tolerance. 
                   na.target = TRUE))$x.hat # hmm, despite ensuring that all possible combinations of the categorical variables were accounted for with 0 values, there were still some NA values in the margin. Although very few. As these NA values almost certainly reflect 0 values anyway, I'm going to set it to allow na values in the margins. 
  
  return(res)
  
}


## 3. carrying out the iterative proportional fitting, by looping the function defined above over all geographies
all_geogs <- lsoa_seed[, unique(lad22cd)]

fitted_ests <- list()
start_time <- Sys.time()
for(i in 1:length(all_geogs)){
  
  input_geog <- all_geogs[i]
  
  res <- ipf_est(input_geog = input_geog)
  
  fitted_ests[[i]] <- res
  
}
end_time <- Sys.time()

time_taken <- end_time - start_time


## 4. reshaping the data into a long data.table
long_geog_list <- vector("list", length(fitted_ests))

for(i in 1:length(all_geogs)){
  
  res <- data.table(reshape2::melt(fitted_ests[[i]]))
  
  geog_code <- all_geogs[i]
  
  res[, lad23cd := geog_code]
  
  long_geog_list[[i]] <- res
  
}

fitted_deaths <- rbindlist(long_geog_list)

colnames(fitted_deaths) <- c("lsoa21cd", "year", "age", "sex", "deaths", "lad23cd")

fitted_deaths <- fitted_deaths[, c("lad23cd", "lsoa21cd", "year", "age", "sex", "deaths")]


## 4. saving the dataset
saveRDS(object = fitted_deaths,
        file = "input_data/intermediate/fitted_lsoa21_deaths.rds")

rm(list = ls())
gc()
