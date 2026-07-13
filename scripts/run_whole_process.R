
### A. create any directories needed
source("functions/fn_check_and_create_dir.R")

check_and_create_dir("input_data/intermediate")
check_and_create_dir("output_data")


### B. creating inputs to go into net flows residual differencing process
source("scripts/1_population/1_a_reading_in_and_cleaning_lsoa21_population.R")
source("scripts/1_population/1_d_constraining_to_la_backseries.R")

source("scripts/2_births/2_a_births_data_prep.R")

source("scripts/3_deaths/3_a_reading_in_and_processing_data.R")
source("scripts/3_deaths/3_b_creating_lsoa11_deaths_estimates.R")
source("scripts/3_deaths/3_c_creating_lsoa21_deaths_estimates.R")
# source("scripts/3_deaths/3_b_creating_the_seed_and_margins_lsoa11.R")
# source("scripts/3_deaths/3_c_creating_death_estimates_with_ipf_lsoa11.R")
# source("scripts/3_deaths/3_d_creating_seed_and_margins_lsoa21.R")
# source("scripts/3_deaths/3_e_creating_death_estimates_with_ipf_lsoa21.R")

### C. create estimated flows from population, births, and deaths above, via residual differencing, at the desired end geography that is determined by the input lookups 
source("scripts/4_flows_general/4_a_calculating_net_flows.R")
source("scripts/4_flows_general/4_b_calculating_net_flows_msoa11_2011.R")
source("scripts/4_flows_general/4_c_getting_lsoa11_outflows.R")
source("scripts/4_flows_general/4_d_getting_base_gross_flows.R")
source("scripts/4_flows_general/4_e_getting_gross_flows_full_backseries.R")


### D. collate all components into one dataset
source("scripts/5_all_datasets/5_c_collating_into_one_dataset_general.R")


