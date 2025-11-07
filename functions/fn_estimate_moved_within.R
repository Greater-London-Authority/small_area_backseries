## this is a function to estimate the number of people who moved address in the past year, within the same geographical area, based on the population of that area
## it is only intended for use at ward-level and below 
## it is based on a linear regression model that was generated using census 2011 table ukmig008
## in this repo, you can see details on the creation of the model in the file "creating_model_for_scaling_flows.R" in the folder scripts/0_misc_prep_scripts
## [of course, if this function were to be moved around into different repos, the line above would need to change - ideally should link out to some public, short repo that goes through the process and creates the model]

estimate_moved_within <- function(input_population){
  
  moved_within_estimate <- (1.330e-03*input_population + (-7.809e-03))^2
  
  return(moved_within_estimate)
  
}

