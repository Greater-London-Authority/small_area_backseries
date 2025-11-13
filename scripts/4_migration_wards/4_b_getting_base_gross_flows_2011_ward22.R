## TO DO - split this script into two. It's too long. Make the first one the script that calculates international outflows, and the second is the one that creates the final base flows. 


## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")

## 1. reading in the data (these were all just downloaded and put in the raw data folder. Need to add a way to read them in remotely. nomis? Put them up publicly somewhere? Is this something we'll put up on the datastore?)

  ### 1.1. census flows
int_in_m <- fread("input_data/raw/CT0496_male_international_in_to_msoa.csv",
                  skip = 1)
int_in_f <- fread("input_data/raw/CT0497_female_international_in_to_msoa.csv",
                  skip = 1)


dom_in_m <- fread("input_data/raw/CT0498_male_domestic_in_to_msoa.csv",
                  skip = 1)
dom_in_f <- fread("input_data/raw/CT0499_female_domestic_in_to_msoa.csv",
                  skip = 1)


dom_out_m <- fread("input_data/raw/CT0543_male_domestic_out_from_msoa.csv",
                   skip = 1)
dom_out_f <- fread("input_data/raw/CT0544_female_domestic_out_from_msoa.csv",
                   skip = 1)

ukmig_msoa <- fread("input_data/raw/ukmig008_msoa.csv")

  ### 1.2. estimated 2011 MYE netflows
mye_net_11 <- readRDS("input_data/intermediate/msoa11_flows.rds")

  ### 1.3. msoa and lsoa 2011
msoa_pop <- readRDS("input_data/raw/population_MSOA11_01_to_20.rds")
lsoa_pop <- readRDS("input_data/raw/population_LSOA11_01_to_20.rds")

msoa_pop <- data.table(msoa_pop)
lsoa_pop <- data.table(lsoa_pop)

msoa_pop <- msoa_pop[year == 2011, ]
lsoa_pop <- lsoa_pop[year == 2011, ]

gc()

colnames(msoa_pop) <- tolower(colnames(msoa_pop))
colnames(lsoa_pop) <- tolower(colnames(lsoa_pop))

colnames(msoa_pop)[7] <- "population_msoa"
colnames(lsoa_pop)[8] <- "population_lsoa"

msoa_pop <- msoa_pop[, -c("lad11cd", "lad11nm")]
lsoa_pop <- lsoa_pop[, -c("lad11cd", "lad11nm")]

  ### 1.3. lookups
lsoa11_ward22_weighted <- readRDS("lookups/lsoa11_ward22_weighted_extended.rds")
lsoa11_ward22_weighted <- lsoa11_ward22_weighted[weight != 0, ]


## 2. reshaping the data

  ### 2.1. getting rid of those with totals and empty columns, filling in the missing rows for the international migration datasets, and then binding them into the same dataset for data processing
dom_out_f <- dom_out_f[, -2]
colnames(dom_out_f)[2:76] <- paste0("V", 2:76) # need to align for the rbind later
int_in_f <- int_in_f[, -77]

int_in_m[, ':=' (sex = "male", type = "international_in")]
int_in_f[, ':=' (sex = "female", type = "international_in")]
dom_in_m[, ':=' (sex = "male", type = "domestic_in")]
dom_in_f[, ':=' (sex = "female", type = "domestic_in")]
dom_out_m[, ':=' (sex = "male", type = "domestic_out")]
dom_out_f[, ':=' (sex = "female", type = "domestic_out")]


int_in_m <- rbind(int_in_m, 
                  data.table(V1 = "E02002158 Wolverhampton 010", # adding the missing ones manually, hardcoded. TO DISCUSS. I think this is ok, because it's in a script and not a function, and we'll only be doing this once. 
                             sex = "male",
                             type = "international_in"), 
                  fill = TRUE)

int_in_m[7201, 2:76] <- 0

int_in_f <- rbind(int_in_f, 
                  data.table(V1 = c("E02001794 Sunderland 004", "E02006184 South Staffordshire 011"),
                             sex = c("female", "female"),
                             type = c("international_in", "international_in")),
                  fill = TRUE)

int_in_f[7200:7201, 2:76] <- 0

all_flows <- rbind(int_in_m, int_in_f, 
                   dom_in_m, dom_in_f,
                   dom_out_m, dom_out_f)


  ### 2.2. adding a column with normal gss codes, and adding the age columns
all_flows[, msoa11cd := tstrsplit(x = V1, split = " ")[1]]

colnames(all_flows)[1] <- "msoa11nm"

colnames(all_flows)[2:76] <- paste0("age_", 1:75)

### 2.3. estimating flows at age 0, rearranging columns, and pivoting longer
all_flows[, age_0 := age_1/2]

all_flows <- all_flows[, c("msoa11cd", "msoa11nm", "sex", "type", paste0("age_", 0:75))]

all_flows <- data.table::melt(all_flows, id.vars = c("msoa11cd", "msoa11nm", "sex", "type"))

all_flows[, age := as.numeric(gsub("age_", "", variable))]
all_flows <- all_flows[, -"variable"]

### 2.4. splitting out ages from 75 and over to 85 and over (really need a dedicated function to do this )
age_lookup <- data.table(
  age_75 = c(0:74, rep(75, 11)),
  age_85 = c(0:85),
  to_divide = c(rep(1, 75), rep(11, 11))
)

setkey(all_flows, "age")
setkey(age_lookup, "age_75")

all_flows <- age_lookup[all_flows, 
                        allow.cartesian = TRUE]

all_flows[, value := value/to_divide]

all_flows <- all_flows[, -c("age_75", "to_divide")]

colnames(all_flows)[colnames(all_flows) == "age_85"] <- "age"

### 2.5. getting total inflows
in_flows <- all_flows[type %in% c("international_in", "domestic_in"), ]

in_flows <- in_flows[, .(inflow = sum(value)),
                     by = list(msoa11cd, msoa11nm, sex, age)]


## 3. getting outflows at msoa level


  ### 3.1. fixing up the msoa net flows dataset
colnames(mye_net_11)[colnames(mye_net_11) == "gross_flows"] <- "net_flows" # because I mislabelled them earlier

mye_net_11 <- mye_net_11[, c("lad23cd", "msoa11cd", "year", "age", "sex", "net_flows")]

  ### 3.2. joining the two datasets
setkey(in_flows, "msoa11cd", "sex", "age")
setkey(mye_net_11, "msoa11cd", "sex", "age")

gross_flows <- in_flows[mye_net_11]

gross_flows[, outflow := inflow - net_flows] # TO DISCUSS. What is the convention for whether outflows are negative or positive in sign? Here, a value of +10 would mean that 10 people flowed out, so the msoa lost a population of 10. 
# TO DISCUSS also. Some of the outflows come out as negative. What do we do about this? Assign a value  of 0? Take the absolute? Something else? 

# NOTE - here is where I should cut this script in two. 

## 4. splitting the msoa outflows into lsoa outflows
### I think the best way to do this, in the first step, is to scale the msoa flows to lsoa by using a population weighted scaling factors. Seems to make sense, as you'd expect inflows and outflows to be influenced by the population, and this is the accepted way anyway. 
### then, we use the modelled approach from the ukmig tables to estimate how much of the flows are within the geography in question. 
### the modelled approach is only total moved within the area, not by age, so we just apportion the total according the existing estimated age-sex structure of migration. 

  ### 4.1. calculating the total population-weighted scaling factors for splitting MSOA down to LSOA

    #### first the age and sex specific ones
setkey(msoa_pop, "sex", "age", "year", "msoa11cd")
setkey(lsoa_pop, "sex", "age", "year", "msoa11cd")

scaling_factors <- lsoa_pop[msoa_pop, 
                            allow.cartesian = TRUE]

scaling_factors <- aggregate_to_lower_max_age(scaling_factors, new_max_age = 85, count_names = c("population_lsoa", "population_msoa"))

scaling_factors[, scaling_factor := population_lsoa/population_msoa]

scaling_factors[is.na(scaling_factor), scaling_factor := 0] # because, in a small number of cases, there is population 0 in msoas/wards for particular ages, which creates NaNs after the division above

scaling_factors <- scaling_factors[, c("msoa11cd", "lsoa11cd", "sex", "age", "year", "scaling_factor")]

    #### then the total population scaling factors, with no disaggregation by age and sex
msoa_pop_total <- msoa_pop[, .(population_msoa = sum(population_msoa)), 
                           by = list(msoa11cd, year)]

lsoa_pop_total <- lsoa_pop[, .(population_lsoa = sum(population_lsoa)),
                           by = list(msoa11cd, lsoa11cd, year)]

setkey(msoa_pop_total, "msoa11cd", "year")
setkey(lsoa_pop_total, "msoa11cd", "year")

scaling_factors_total <- lsoa_pop_total[msoa_pop_total]

scaling_factors_total[ , scaling_factors := population_lsoa/population_msoa]

scaling_factors_total <- scaling_factors_total[, c("msoa11cd", "lsoa11cd", "scaling_factors")]

  ### 4.2. scaling msoa flows down to lsoa flows (this is something that needs to be a function too. Scaling from one geography down to another, by population). 
setkey(gross_flows, "msoa11cd", "sex", "age", "year")

setkey(scaling_factors, "msoa11cd", "sex", "age", "year")
scaling_factors[is.na(scaling_factor), ]
gross_flows_lsoa <- scaling_factors[gross_flows]

gross_flows_lsoa[, inflow := scaling_factor*inflow]
gross_flows_lsoa[, outflow := scaling_factor*outflow]

gross_flows_lsoa <- gross_flows_lsoa[, -c("scaling_factor", "net_flows")]

  ### 4.3. scaling total number of within msoa flows to lsoa (which will be the amount we need to allocate for each lsoa) and joining with the gross flows dataset # NOTE - I think it would be meaningful to get this all into one function. A fairly discrete, definable thing. 
ukmig_msoa <- ukmig_msoa[, c(2, 4)] # fixing up and joining on msoa within-area flows in the few lines below

colnames(ukmig_msoa) <- c("msoa11cd", "moved_within_msoa")

setkey(ukmig_msoa, "msoa11cd")
setkey(scaling_factors_total, "msoa11cd")

ukmig_msoa <- scaling_factors_total[ukmig_msoa]

ukmig_msoa[, moved_within_msoa := moved_within_msoa*scaling_factors]

setkey(ukmig_msoa, "msoa11cd", "lsoa11cd")
setkey(gross_flows_lsoa, "msoa11cd", "lsoa11cd")

gross_flows_lsoa <- ukmig_msoa[gross_flows_lsoa]

gross_flows_lsoa <- gross_flows_lsoa[, -"scaling_factors"]

lsoa_pop_total[, moved_within_lsoa := estimate_moved_within(population_lsoa)] # next estimating the total flow within lsoas, and subtracting that, because we don't want that to be added to flows between lsoas within an msoa

setkey(gross_flows_lsoa, "msoa11cd", "lsoa11cd", "year")
setkey(lsoa_pop_total, "msoa11cd", "lsoa11cd", "year")


gross_flows_lsoa <- lsoa_pop_total[gross_flows_lsoa]

gross_flows_lsoa[, moved_within_msoa := moved_within_msoa - moved_within_lsoa]

gross_flows_lsoa <- gross_flows_lsoa[, c("lad23cd", "msoa11cd", "msoa11nm", "lsoa11cd", "year", "sex", "age", "moved_within_msoa", "population_lsoa", "inflow", "outflow")] # need to keep lsoa population, to get msoa21/ward22 population estimate later for estimating the within msoa21cd flows to be subtracted

  ### 4.4. splitting the flows of people within msoas, which are already allocated across lsoas in the subsection above, from totals to single year of age by sex, and getting the final figures for outflows and inflows
  ### we have the number of people who have moved within the msoa in the past year, but there is no age or sex aggregation. So as well as distribute that number lsoas, we also have to distribute it across single year of age and sex
  ### by the lsoa-level flows by age that we have, just creating an age/sex distribution for each lsoa and using that to allocate. 

gross_flows_lsoa <- scale_down_total_variable(dataset = gross_flows_lsoa, variable_to_get_age_distribution = "inflow", 
                                              variable_to_scale_down = "moved_within_msoa", categories_to_keep = c("lsoa11cd", "year"))

gross_flows_lsoa <- scale_down_total_variable(dataset = gross_flows_lsoa, variable_to_get_age_distribution = "outflow", 
                                              variable_to_scale_down = "moved_within_msoa", categories_to_keep = c("lsoa11cd", "year"))

gross_flows_lsoa[, inflow := inflow + moved_within_msoa_scaled_by_inflow]
gross_flows_lsoa[, outflow := outflow + moved_within_msoa_scaled_by_outflow] # NOTE - when this process is fixed so that it has the flexibility to accommodate any geography you feed in, these are the outputs I'll need to save, and then a separate script reads them in and aggregates them according to the lookup you use

## NOTE - below is the only bit I need to change to adjust the process from one geography to another
## 5. getting ward22 flows

  ### 5.1. aggregating lsoa11 up to ward22
gross_flows_lsoa <- gross_flows_lsoa[, c("lsoa11cd", "year", "sex", "age", "population_lsoa", "inflow", "outflow")]

gross_flows_lsoa <- gross_flows_lsoa[, .(year = min_year:max_year), 
                                     by = list(lsoa11cd, sex, age, population_lsoa, inflow, outflow)]

gross_flows_ward <- aggregate_geographies_weighted(data = gross_flows_lsoa, lookup = lsoa11_ward22_weighted, # TO CHECK - pretty sure that this function is right, but double check just in case. It's a new function. 
                                                   geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd", geog_to_lookup = "ward22cd", # A to-do. I've estimated ward22 population, when I can just get it directly. I don't think this will make much difference, but still..probably something to fix the make the process a bit better. 
                                                   count_names = c("population_lsoa", "inflow", "outflow"))

colnames(gross_flows_ward)[colnames(gross_flows_ward) == "population_lsoa"] <- "population_ward"

  ### 5.2. subtracting the within ward flows
gross_flows_ward[, moved_within_ward := estimate_moved_within(population_ward)]

gross_flows_ward <- scale_down_total_variable(dataset = gross_flows_ward, # also to double check. is it definitely within ward flows that we want to subtract? I think so, but must be sure
                                              variable_to_get_age_distribution = "inflow", variable_to_scale_down = "moved_within_ward",
                                              categories_to_keep = c("ward22cd", "year"))

gross_flows_ward <- scale_down_total_variable(dataset = gross_flows_ward,
                                              variable_to_get_age_distribution = "outflow", variable_to_scale_down = "moved_within_ward",
                                              categories_to_keep = c("ward22cd", "year"))

gross_flows_ward[, inflow := inflow - moved_within_ward_scaled_by_inflow]
gross_flows_ward[, outflow := outflow - moved_within_ward_scaled_by_outflow]


gross_flows_ward <- gross_flows_ward[, c("ward22cd", "year", "sex", "age", "inflow", "outflow")]


## 6. fixing up and saving the gross flows file
file_path <- paste0("input_data/intermediate/base_gross_flows_ward22cd_2011_", max_year, ".rds")

saveRDS(object = gross_flows_ward,
        file = file_path)

rm(list = ls())
gc()




