
add_inflows_to_population_at_risk <- function(std_pop_at_risk, inflows) {
  
  out_df <- std_pop_at_risk %>%
    left_join(inflows, by = join_cols) %>%
    mutate(population_at_risk = population_at_risk + inflow) %>%
    select(-inflow) 
  
  return(out_df)
}