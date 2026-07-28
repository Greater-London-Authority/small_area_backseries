
get_base_out_mig_rates <- function(outflows, pop_at_risk, base_years) {
  
  out_df <- outflows %>%
    filter(year %in% base_years) %>%
    left_join(pop_at_risk, by = join_cols) %>%
    group_by(across(-any_of(c("year", "outflow", "population_at_risk")))) %>%
    summarise(population_at_risk = sum(population_at_risk), 
              outflow = sum(outflow), 
              .groups = "drop") %>%
    mutate(population_at_risk = case_when(
      population_at_risk < outflow ~ outflow,
      population_at_risk < 1 ~ 1,
      TRUE ~ population_at_risk
    )) %>%
    mutate(out_rate = case_when(
      outflow/population_at_risk > rate_max ~ rate_max,
      TRUE ~ outflow/population_at_risk
    )) %>%
    select(-c(population_at_risk, outflow))
  
  return(out_df)
}


