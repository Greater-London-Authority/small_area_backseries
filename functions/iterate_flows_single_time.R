

iterate_flows_single_time <- function(std_pop_at_risk, 
                                      inflows,
                                      outflows,
                                      base_years) {
  
  population_at_risk <- add_inflows_to_population_at_risk(
    std_pop_at_risk, 
    inflows
  ) 
  
  base_out_mig_rates <- get_base_out_mig_rates(
    outflows, 
    population_at_risk, 
    base_years_for_out_rate_prior
  )
  
  base_flows <- create_base_flows(outflows, 
                                  inflows, 
                                  base_out_mig_rates,
                                  population_at_risk)
  
  modelled_flows <- base_flows %>%
    mutate(model_flows = optimise_gross_flows(base_inflow, base_outflow, net_flow)) %>%
    unnest_wider(col = model_flows) 
  
  modelled_inflow <- modelled_flows %>%
    select(area_code, geography, scenario, year, age, sex, gss_code, inflow)
  
  modelled_outflow <- modelled_flows %>%
    select(area_code, geography, scenario, year, age, sex, gss_code, outflow)
  
  original_turnover <- sum(inflows$inflow) + sum(outflows$outflow)
  new_turnover <- sum(modelled_inflow$inflow) + sum(modelled_outflow$outflow)
  
  prop_change_turnover <- (new_turnover - original_turnover)/original_turnover
  
  out_list <- list(inflow = modelled_inflow,
                   outflow = modelled_outflow,
                   change_turnover = prop_change_turnover)
  
  return(out_list)
}





