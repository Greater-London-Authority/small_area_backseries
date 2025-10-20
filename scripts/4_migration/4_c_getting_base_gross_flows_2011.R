
## 0. libraries and functions
library(data.table)


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

  ### 1.3. msoa and lsoa 2011 population (using midyear for this and not census. TO DISCUSS - is this right?)
msoa_pop <- readRDS("input_data/raw/population_MSOA11_01_to_20.rds")
lsoa_pop <- fread("input_data/intermediate/mid_year_rebased_20112022_lsoa21.csv")

msoa_pop <- data.table(msoa_pop)

msoa_pop <- msoa_pop[year == 2011, ]
lsoa_pop <- lsoa_pop[year == 2011, ]

  ### 1.4. adding msoa11 onto lsoa21
lsoa21_msoa11_lookup <- fread("lookups/2021_lsoa_2011_msoa.csv")

setkey(lsoa21_msoa11_lookup, "lsoa21cd")
setkey(lsoa_pop, "lsoa21cd")

lsoa_pop <- lsoa21_msoa11_lookup[lsoa_pop]


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
                  data.table(V1 = "E02002158 Wolverhampton 010", # adding the msising ones manually, hardcoded. TO DISCUSS. I think this is ok, because it's in a script and not a function, and we'll only be doing this once. 
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

all_flows <- melt(all_flows, id.vars = c("msoa11cd", "msoa11nm", "sex", "type"))

all_flows[, age := as.numeric(gsub("age_", "", variable))]
all_flows <- all_flows[, -"variable"]

  ### 2.4. getting total inflows
in_flows <- all_flows[type %in% c("international_in", "domestic_in"), ]

in_flows <- in_flows[, .(inflow = sum(value)),
                     by = list(msoa11cd, msoa11nm, sex, age)]


## 3. getting outflows at msoa level

  ### 3.1. fixing up the msoa net flows dataset
mye_net_11[age > 75, age := 75]

mye_net_11 <- mye_net_11[, .(net_flows = sum(gross_flows)),
                         by = .(lad23cd, msoa11cd, year, age, sex)]

  ### 3.2. joining the two datasets
setkey(in_flows, "msoa11cd", "sex", "age")
setkey(mye_net_11, "msoa11cd", "sex", "age")

gross_flows <- in_flows[mye_net_11]

gross_flows[, outflow := inflow - net_flows] # TO DISCUSS. What is the convention for whether outflows are negative or positive in sign? Here, a value of +10 would mean that 10 people flowed out, so the msoa lost a population of 10. 
                                             # TO DISCUSS also. Some of the outflows come out as negative. What do we do about this? Assign a value  of 0? Take the absolute? Something else? 


## 4. splitting the msoa outflows into lsoa outflows
### I think the best way to do this, in the first step, is to scale the msoa flows to lsoa by using a population weighted scaling factors. Seems to make sense, as you'd expect inflows and outflows to be influenced by the population, and this is the accepted way anyway. 
### and then, as for accounting for flows within the msoa, there is a column in UKmig that captures this - people who responded that they had a different address from this time last year, but still lived in the same msoa. So I'm just going to allocate this across the lsoas within each msoa, and as above will allocate it according the population of each lsoa and then according to the distribution by age of migration (because there is no age disaggregation at all in ukmig). 
### a flaw in the approach above is that some of the people who moved within the same msoa will also have moved within the same lsoa. And therefore shouldn't be added to those who have migrated in and out of lsoas. I don't think adjustment will be very big and I haven't done it yet. TO DISCUSS - what is best way to do this? 
### TO DISCUSS - of course, those who have moved within the msoa have both moved out of somewhere, and into somewhere. So the total figure needs to be added to both inflows and outflows, right? 

  ### 4.1. calculating the scaling factors for splitting MSOA down to LSOA
colnames(msoa_pop) <- tolower(colnames(msoa_pop))
colnames(lsoa_pop) <- tolower(colnames(lsoa_pop))

colnames(msoa_pop)[7] <- "population_msoa"
colnames(lsoa_pop)[9] <- "population_lsoa"

msoa_pop[, -c("lad11cd", "lad11nm")]
lsoa_pop[, -c("lad21cd", "lad21nm")]

setkey(msoa_pop, "sex", "age", "year", "msoa11cd")
setkey(lsoa_pop, "sex", "age", "year", "msoa11cd")

scaling_factors <- lsoa_pop[msoa_pop, 
                            allow.cartesian = TRUE]

scaling_factors[age > 75, age := 75] # DEFINITELY need a function to aggregate the end ages to a broad age band (note - have already done this, just need to put it in now)

scaling_factors <- scaling_factors[, .(population_lsoa = sum(population_lsoa), population_msoa = sum(population_msoa)),
                                   by = .(sex, age, year, lsoa21cd, msoa11cd)]

scaling_factors[, scaling_factor := population_lsoa/population_msoa]

scaling_factors <- scaling_factors[, c("msoa11cd", "lsoa21cd", "sex", "age", "year", "scaling_factor")]

  ### 4.2. adding the total number who have moved within the msoa, from the uk mig tables. We need to apportion this from msoa to lsoa by population too. We will also need to split this by age and sex, which we do later in section 4. 
ukmig_msoa <- ukmig_msoa[, c(2, 4)]

colnames(ukmig_msoa) <- c("msoa11cd", "moved_within")

setkey(ukmig_msoa, "msoa11cd")
setkey(gross_flows, "msoa11cd")

gross_flows <- ukmig_msoa[gross_flows]

  ### 4.3. scaling msoa flows down to lsoa flows (this is something that needs to be a function too. Scaling from one geography down to another, by population). 
setkey(gross_flows, "msoa11cd", "sex", "age", "year")

setkey(scaling_factors, "msoa11cd", "sex", "age", "year")

gross_flows_lsoa <- scaling_factors[gross_flows]

gross_flows_lsoa[, inflow := scaling_factor*inflow]
gross_flows_lsoa[, outflow := scaling_factor*outflow]
gross_flows_lsoa[, moved_within := scaling_factor*moved_within]

gross_flows_lsoa <- gross_flows_lsoa[, -c("scaling_factor", "net_flows")]


  ### 4.4. splitting the flows of people within msoas, which are already allocated across lsoas, from totals to single year of age by sex
  ### we have the number of people who have moved within the msoa in the past year, but there is no age or sex aggregation. So as well as distribute that number lsoas, we also have to distribute it across single year of age and sex
  ### by the lsoa-level flows by age that we have, just creating an age/sex distibution for each lsoa and using that to allocate. 

gross_flows_lsoa[, all_ages_inflows := .(sum(inflow)), # calculating total inflow for lsoa, summing over age and sex
            by = .(lsoa21cd, year)] # TO-DO - after a bit of time, double check that this line is correct. I do think it is, but if it were wrong it would mess everything up a way that may not be immediately obvious. 

gross_flows_lsoa[, all_ages_outflows := .(sum(outflow)), # calculating total outflow for lsoa, summing over age and sex
            by = .(lsoa21cd, year)]

gross_flows_lsoa[, inflow_age_scaling := inflow/all_ages_inflows] # getting the scaling factors for inflow and outflow, by dividing each value for age and sex by the total in or out flow for that lsoa
gross_flows_lsoa[, outflow_age_scaling := outflow/all_ages_outflows]


gross_flows_lsoa[, outflow_within := moved_within*outflow_age_scaling] # dividing the total within-msoa flow to be allocated to each lsoa, by the age-sex scaling factors derived above
gross_flows_lsoa[, inflow_within := moved_within*inflow_age_scaling]

gross_flows_lsoa[, inflow := inflow + inflow_within] # adding the within msoa flows to the lsoa flows, to get the final gross flow values by msoa
gross_flows_lsoa[, outflow := outflow + outflow_within]


## 5. fixing up and saving the gross flows file

gross_flows_lsoa_tosave <- gross_flows_lsoa[, c("lad23cd", "msoa11cd", "lsoa21cd", "sex", 
                                         "age", "year", "inflow", "outflow")]

saveRDS(object = gross_flows_lsoa_tosave,
        file = "input_data/intermediate/base_gross_flows_lsoa21cd_2011.rds")


    #### bit of qa, calculating the percentage of flows that are from within the msoa. 10% ish, with a pretty wide range, is about right. Other small bits of qa too. 

gross_flows_lsoa[, perc_in := 100*(inflow_within/inflow)]
gross_flows_lsoa[, perc_out := 100*(outflow_within/outflow)]

hist(gross_flows_lsoa[, perc_in], breaks = 1000)
hist(gross_flows_lsoa[, perc_out], breaks = 1000) # some very, very large values. I think, because outflows were estimated by differencing, some of the values are strange and off, even if the great majority worked just fine. The more specific issue here is that some outflows ended up incredibly close to 0, meaning that when out divide by them they get enormous. Pretty sure anyway. 

hist(gross_flows_lsoa[, outflow], breaks = 1000) ## some too-large values for both inflows and outflows. 
hist(gross_flows_lsoa[, inlow], breaks = 1000) ## 

hist(gross_flows_lsoa[, outflow_within], breaks = 1000)

plot(gross_flows_lsoa[lsoa21cd == "E01001146" & sex == "male", outflow_age_scaling], type = "l") # small number of slightly negative outflows. What to do with them? 

unique(gross_flows_lsoa$lsoa21cd)
