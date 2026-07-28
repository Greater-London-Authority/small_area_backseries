
create_base_flows <- function(outflows, 
                              inflows, 
                              base_out_rates, 
                              pop_at_risk) {
  
  base_outflows <- outflows %>%
    left_join(pop_at_risk, by = join_cols) %>%
    left_join(base_out_rates, by = join_cols_base) %>%
    mutate(base_outflow = out_rate * population_at_risk) %>%
    select(-c(population_at_risk, out_rate))
  
  base_flows <- base_outflows %>%
    left_join(inflows, by = join_cols) %>%
    mutate(net_flow = inflow - outflow) %>%
    select(-c(outflow)) %>%
    rename(base_inflow = inflow) %>%
    mutate(base_inflow = case_when(
      base_inflow < 0.1 ~ 0.1,
      TRUE ~ base_inflow
    )) %>%
    mutate(base_outflow = case_when(
      base_outflow < 0.1 ~ 0.1,
      TRUE ~ base_outflow
    ))
  
  return(base_flows)
}

