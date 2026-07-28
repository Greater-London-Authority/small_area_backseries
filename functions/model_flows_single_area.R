
model_flows_single_area <- function(std_pop_at_risk, 
                                    inflows,
                                    outflows,
                                    base_years,
                                    max_iterations = 100) {
  
  change_turnover <- 1
  j <- 1
  
  i_inflow <- inflows
  i_outflow <- outflows
  
  while((change_turnover > 0.003) & (j <= max_iterations)){
    
    modelled_flows <- iterate_flows_single_time(std_pop_at_risk, 
                                                i_inflow,
                                                i_outflow,
                                                base_years_for_out_rate_prior)
    
    change_turnover <- modelled_flows$change_turnover
    
    i_inflow <- modelled_flows$inflow
    i_outflow <- modelled_flows$outflow
    
    j <- j + 1
  }
  
  out_df <- bind_rows(i_inflow %>%
                        rename(value = inflow) %>%
                        mutate(component = "inflow"),
                      
                      i_outflow %>%
                        rename(value = outflow) %>%
                        mutate(component = "outflow"))
  
  return(out_df)
}

