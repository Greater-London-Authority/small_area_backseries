
# TO DO - a lot of this should be split into functions. Not one function, because it'd be way too long and messy, but a few smaller functions.
# because, yes, now I have to redo the whole thing for a single year of MSOA data. 

## 0. libraries and functions 
library(data.table)
library(gsscoder)

## 1. reading in the data
population <- fread("input_data/intermediate/mid_year_rebased_20112022_lsoa21.csv")

deaths <- readRDS("input_data/intermediate/fitted_lsoa_deaths.rds")

births <- readRDS("input_data/raw/births_oa11_mid_2002_2020.rds")


## 2. calculating the gross flows possible with only population and deaths

  ### 2.1. fixing up some of the input datasets
population[age > 85, age := 85]

population[, cohort := year - age]
deaths[, cohort := year - age]

population <- population[, .(population = sum(population)),
                         by = list(lad21cd, lad21nm, lsoa21cd, lsoa21nm, year, age, sex, cohort)]

population <- recode_gss(df_in = population[, -"lad21nm"], 
                         col_code = "lad21cd", col_data = "population",
                         recode_from_year = 2021, recode_to_year = 2023)

population <- data.table(population)

colnames(population)[1] <- "lad23cd"

  ### 2.2. getting the change in cohort from one year to the next
  ### QUESTION on the residual change method. When tracking cohorts, deaths up to mid year does not map onto any one cohort, but is spread across 2. How do we account for that? Or do we try to account for that? 
  ### discussed this and it probably is accounted for...but need to double check and find the exact definition. 

population_prev <- copy(population)

population_prev[, year := year + 1]

population_prev <- population_prev[, -"age"]
colnames(population_prev)[7] <- "population_last_year"

setkey(population, "lad23cd", "lsoa21cd", "lsoa21nm", "year", "sex", "cohort")
setkey(population_prev, "lad23cd", "lsoa21cd", "lsoa21nm", "year", "sex", "cohort")

flows <- population_prev[population]

flows <- flows[!is.na(population_last_year), ]
flows[!is.na(population_last_year), gross_flows := population - population_last_year]

flows <- flows[year <= 2020, ]

hist(flows$gross_flows, breaks = 1000)
hist(flows[gross_flows < 20 & gross_flows > - 20, gross_flows], breaks = 100)

100*(nrow(flows[gross_flows < 10 & gross_flows > -10, ])/nrow(flows)) # 98% of the gross flows are less than 10

look <- flows[gross_flows > 100 | gross_flows < -100, ] # TO DISCUSS. Some flows are enormous. But they're almost all around student ages. Do we expect some lsoa level flows to be this big for students? 

setkey(deaths, "lad23cd", "lsoa21cd", "year", "sex", "age", "cohort")
setkey(flows, "lad23cd", "lsoa21cd", "year", "sex", "age", "cohort")

flows <- deaths[flows]

flows[, gross_flows := gross_flows + deaths]

flows[deaths > population_last_year, ] # TO DISCUSS .Nearly a million cells - out of about 54 million - where deaths is higher than the population last year. Possibly an issue, although actually, not impossible. People could have moved in after mid year last year and then died before mid year this year. Which might make sense for certain areas, depending on the characteristics? 


## 3. calculating flows for those aged 0. 

  ### 3.1. fitting births to lsoa21
oa11_lsoa21_lookup <- fread("lookups/2011_oa_2022_lsoa_msoa_la.csv") # all of the below can be done, and should be done, in a function. The joining and aggregating, that is. 

oa11_lsoa21_lookup <- oa11_lsoa21_lookup[, c("oa11cd", "lsoa21cd")] # this lookup is missing 157 lsoas, out of over 180,000. 

births <- data.table(births)

setkey(oa11_lsoa21_lookup, "oa11cd")
setkey(births, "OA11CD")

births <- oa11_lsoa21_lookup[births]

births <- births[, .(births = sum(births)),
                 by = list(lsoa21cd, sex, year)]


  ### 3.2. joining births with population, then joining deaths, and finally getting the gross flows via residuals
births[, age := 0]
births[, cohort := year]

population_0 <- population[age == 0, ]

setkey(population_0, "lsoa21cd", "year", "age", "sex", "cohort")
setkey(births, "lsoa21cd", "year", "age", "sex", "cohort")

flows_0 <- births[population_0]

flows_0[is.na(births), births := 0]

setkey(flows_0, "lad23cd", "lsoa21cd", "year", "sex", "age", "cohort")
setkey(deaths, "lad23cd", "lsoa21cd", "year", "sex", "age", "cohort")

flows_0 <- deaths[flows_0]

flows_0[, gross_flows := population - births + deaths]

hist(flows_0[, deaths], breaks = 1000)

    #### quick side piece of analysis. In flows created above, some of the gross flows come out as heavily negative, because births are estimated to be very high. 
    #### if you look at which places had those very high estimates of births, they tended to be in the same areas (city, town, district, etc) that were missing lsoas in the oa11 - lsoa21 lookup. 
    #### meaning, what I really think happened, was that there were some lsoas that had too many oas fitted to them, because of the quirks you get in fitting geographies using population weighted centroids. So then, births are heavily overestimated for a small number of lsoas and consequently gross outflows are too. 
    #### TO DISCUSS. Do we need an address-weighted oa11-lsoa21 lookup to avoid this? Or something else? Or does it matter if a small number of lsoas have unrealistic numbers? 
hist(flows_0[, deaths], breaks = 1000)

check <- births[population_0]
look_2 <- check[is.na(births) & !(year %in% c(2021, 2022)), ] # checking which have NA values for births, i.e. those lsoas that weren't in the lookup. 

hist(flows_0[, gross_flows], breaks = 100)

  ### end of side piece of analysis

  ### 3.3. binding flows for age 0 onto the rest of the flows
flows_0[, population_last_year := NA]

flows[, births := NA]

flows <- flows[, c("lad23cd", "lsoa21cd", "lsoa21nm", "year", "age", "cohort", "sex", "births", "deaths", "population_last_year", "population", "gross_flows")]
flows_0 <- flows_0[, c("lad23cd", "lsoa21cd", "lsoa21nm", "year", "age", "cohort", "sex", "births", "deaths", "population_last_year", "population", "gross_flows")]

flows <- rbind(flows_0, flows)

### now, check. Is there anything I'm missing at the births/starting cohort end?
flows <- flows[year %in% 2012:2020, ]

table(flows$year, flows$age) # looks like all ages and years are covered? But I'm sure there were meant to be two things you get from births....
# TO DISCUSS - anything else from births? 

nas <- flows[is.na(gross_flows), ]

nas[, unique(lsoa21cd)] # TO DISCUSS. The gross flow values that are NA are all accounted for by the missing lsoas in the oa11_lsoa21 lookup. Need to come up with a solution.


## 4. the end cohort

## oh no....have been mixing up net and gross migration this whole time. Doesn't matter, just need to rename them. Continue to misname them for now, for consistency, and then change them all at once later. 
 
 ### 4.1. creating the dataset of the previous cohort of 84 and 85+, which in the following year will become 85+
population_end <- population[age %in% 84:85, ]

population_end_prev <- copy(population_end)

population_end_prev <- dcast(population_end_prev, lad23cd + lsoa21cd + lsoa21nm + year + sex ~ age)

colnames(population_end_prev)[6:7] <- c("population_prev_84", "population_prev_85")

population_end_prev[, year := year + 1]

  ### 4.2. joining the two datasets
population_end <- population_end[age == 85, ]

setkey(population_end, "lad23cd", "lsoa21cd", "lsoa21nm", "year", "sex")
setkey(population_end_prev, "lad23cd", "lsoa21cd", "lsoa21nm", "year", "sex")

flows_end <- population_end_prev[population_end]
flows_end <- flows_end[year %in% 2012:2020, ]

  ### 4.3. joining deaths and calculating flows
setkey(flows_end, "lad23cd", "lsoa21cd", "year", "sex", "age", "cohort")

flows_end <- deaths[flows_end]

flows_end[, gross_flows := population - population_prev_85 - population_prev_84 + deaths]

  ### 4.4. adding the end cohort on to the rest of the flows
flows_end[, population_last_year := population_prev_85 + population_prev_84]
flows_end[, births := NA]

flows_end <- flows_end[, -c("population_prev_85", "population_prev_84")]

flows_end <- flows_end[, c("lad23cd", "lsoa21cd", "year", "age", "sex", "deaths", "cohort", "births", "lsoa21nm", "population", "gross_flows", "population_last_year")]

flows <- flows[age != 85, ]

flows_fin <- rbind(flows, flows_end)


## 5. saving the final dataset

saveRDS(object = flows_fin,
        file = "input_data/intermediate/net_flows_lsoa21cd.rds")


## quick QA by eye. 

par(mfrow = c(2, 4))

ages_to_plot <- c(0, 1, 2, 3, 
                  24, 83, 84, 85)

for(i in 1:length(ages_to_plot)){
  
  age_to_plot <- ages_to_plot[i]
  
  hist(flows_fin[age == age_to_plot, gross_flows], 
       breaks = 1000, main = age_to_plot, xlim = c(-100, 100))
  
  
}

par(mfrow = c(1, 1))




