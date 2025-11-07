## need to add explanation/documentation for this function. Same as aggregate_geographies_2 but works with address-weighted lookups
## also, should put in some more time to fully understand .SDcols intuitively, because it's extremely useful! 

aggregate_geographies_weighted <- function(data, lookup,
                                    geog_from_data, geog_from_lookup,
                                    geog_to_lookup, count_names, 
                                    year_col = "year", weight_col = "weight"){ 
  
  ## making sure everything is in a data.table
  data <- data.table(data)
  lookup <- data.table(lookup)
  
  ## in the lookup, only keeping geography columns that we're using, as well as the year column
  look_keep <- c(geog_from_lookup, geog_to_lookup, year_col, weight_col)
  lookup <- lookup[,..look_keep]
  
  ## joining the dataset with the geographical lookup (we're assuming in this function that there are year columns, in both the dataset and the address-weighted lookup)
  setkeyv(data, c(geog_from_data, year_col))
  
  setkeyv(lookup, c(geog_from_data, year_col))
  
  data <- lookup[data]
  
  data <- data[,-..geog_from_lookup]
  
  ## multiplying the value by the weight, getting rid of the weight
  data[, (count_names) :=  lapply(.SD, function(x){x*get(weight_col)}),
                       .SDcols = c(count_names)]
  
  data <- data[, -..weight_col]
  
  ## the final aggregation, from lower geographies to higher geographies
  to_rem <- c(count_names) # name of the count variable
  
  by_cols <- colnames(data)[!(colnames(data) %in% to_rem)] # getting a vector of columns to aggregate over. This is just all of the columns other than the count column(s). 
  
  data <- data[,lapply(.SD, sum, na.rm = TRUE), # applying sum function to each column specified in SD cols
               .SDcols = c(count_names), # setting SDcols. In this case, only the count column(s)
               by = by_cols] # the columns that we want to aggregate over/by. 
  
  return(data)
  
}

