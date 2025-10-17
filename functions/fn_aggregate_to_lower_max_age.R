
## a short function to aggregate a dataset with a column denoting single year of age to a new maxiumum age. 
## so, if maximum age is 90, this function will aggregate it such that the new maximum age is a new age - any age, as long as it's lower than the previous maximum age

aggregate_to_lower_max_age <- function(input_data, age_col = "age", new_max_age, count_names){
  
  input_data[get(age_col) > new_max_age, (age_col) := new_max_age]
  
  by_cols <- colnames(input_data)[!(colnames(input_data) %in% count_names)]
  
  output_data <- input_data[,lapply(.SD, sum, na.rm = TRUE), # applying sum function to each column specified in SD cols
                            .SDcols = c(count_names), # setting SDcols. In this case, only the count column(s)
                            by = by_cols] # the columns that we want to aggregate over/by. 
  
  return(output_data)
  
}
