## document in full later
## important: can't put a set of hierarchical variables in cat_cols. E.g. geographies at different levels. Only include the lowest. 
## also assumes that there is a numerical variable, but that's probably fairly safe



get_missing_categories <- function(dataset, cat_cols, value_col){
  
  ### extracting all categories individually, getting the unique values for each
  cat_list <- list()
  
  for(i in 1:length(cat_cols)){
    
    cat_name <- cat_cols[i]
    
    cat_unique <- dataset[, unique(get(cat_name))]
    
    cat_list[[i]] <- cat_unique
    
  }
  
  names(cat_list) <- cat_cols
  
  ### cross joining them to get all possible combinations
  all_category_combinations <- do.call(CJ, c(cat_list, sorted = FALSE))
  
  ### joining with the input dataset, getting missing ones (they will be the ones with NA values in the value columns)
  joined <- merge(dataset, all_category_combinations, by = cat_cols, all = TRUE)
  
  keep_cols <- c(cat_cols, value_col)
  missing <- joined[is.na(get(value_col)), ..keep_cols]
  
  return(missing)
  
}
