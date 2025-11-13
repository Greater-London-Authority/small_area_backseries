
### create any directories needed
check_and_create_dir("input_data/intermediate")
check_and_create_dir("output_data")


### prep scripts (actually no point for now. They're too messy. And also, they mostly create lookups and a few other processes that can just be loaded up with the repo, and don't need a script to be run within the repo)


### creating general inputs, to be aggregated to higher geographies
source("scripts/1_population/1_a_reading_in_and_cleaning_lsoa21_population.R")

source("scripts/2_births/2_a_births_data_prep.R")

source("scripts/3_deaths/3_a_reading_in_and_processing_data.R")
source("scripts/3_deaths/3_b_creating_the_seed_and_margins_lsoa11.R")
source("scripts/3_deaths/3_c_creating_death_estimates_with_ipf_lsoa11.R")
source("scripts/3_deaths/3_d_creating_seed_and_margins_lsoa21.R")
source("scripts/3_deaths/3_e_creating_death_estimates_with_ipf_lsoa21.R")


### running process for msoa21cd
source("scripts/4_migration/4_a_calculating_net_flows_msoa21.R")
source("scripts/4_migration/4_b_getting_base_gross_flows_2011.R")
source("scripts/4_migration/4_c_getting_gross_flows_full_backseries.R")

source("scripts/5_all_datasets/5_a_collating_into_one_dataset_msoa21.R")


### running process for ward22cd
source("scripts/4_migration_wards/4_a_calculating_net_flows_ward22.R")
source("scripts/4_migration_wards/4_b_getting_base_gross_flows_2011_ward22.R")
source("scripts/4_migration_wards/4_c_getting_gross_flows_full_backseries_ward22.R")

source("scripts/5_all_datasets/5_b_collating_into_one_dataset_ward22.R")


