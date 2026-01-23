
## this function is intended to _
## 

extend_lookup_and_add_weight <- function(lookup, year_start, year_end, add_weight = TRUE){
  
  geogs <- colnames(lookup)

  lookup_extended <- lookup[, .(year = year_start:year_end),
                            by = geogs]
  
  if(add_weight){
    
    lookup_extended[, weight := 1]
    
  }
  
  return(lookup_extended)
  
}

