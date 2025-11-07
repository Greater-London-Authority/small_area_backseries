## this function is intended for the situation where you have a total quantity for a geography, not disaggregated by age or sex
## where you then join this quantity onto another dataset where there is age-sex disaggregation, and you want to allocate the total quantity for the geography by the existing age-sex distribution
## it assumes that all of the variables are in the dataset already, and that the total is joined on to each row before having been disaggregated by age and sex
## you have to specify the numerical variable in the dataset that an age sex distribution is already there for. It then applies this same age and sex distribution to the total quantity you want to split
## "dataset" is the name of the dataset this operation is being applied to
## "variable_to_get_age_distribution" is the numerical variable that already has an age-sex distribution
## "variable_to_scale_down" is the variable that there is only a total for, that we want to scale down by age and sex
## "categories_to_keep" is the (probably misnamed) argument that specifies the categorical variables to aggregate over. This must be all of the categorical variables ASIDE FROM the age and sex variables. This is because we want to aggregate by everything that's not age and sex, so that we have a total to divide all of the  age and sex specific numbers for, so that we have age-sex specific rates to apply. 

scale_down_total_variable <- function(dataset, variable_to_get_age_distribution, variable_to_scale_down, categories_to_keep){
  
  dataset[, all_ages := lapply(.SD, sum), 
          .SDcols = variable_to_get_age_distribution, 
          by = categories_to_keep] 
  
  dataset[, age_scaling := get(variable_to_get_age_distribution)/all_ages]
  
  scaled_colname <- paste0(variable_to_scale_down, "_scaled_by_", variable_to_get_age_distribution)
  
  dataset[, (scaled_colname) := get(variable_to_scale_down)*age_scaling]
  
  dataset <- dataset[, -c("all_ages", "age_scaling")]
  
  return(dataset)
  
}




