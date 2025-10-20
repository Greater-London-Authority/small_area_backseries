
## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

## 1. reading in data, narrowing down to the years we need, aggregating lsoa11 to msoa11

  ### 1.1. reading in data and lookup
population <- readRDS("input_data/raw/population_MSOA11_01_to_20.rds")
population <- data.table(population)
colnames(population) <- tolower(colnames(population))

deaths <- readRDS("input_data/intermediate/fitted_lsoa11_deaths.rds")

births <- data.table(readRDS("input_data/raw/births_oa11_mid_2002_2020.rds"))

all_11_lookup <- fread("lookups/2011_oa_lsoa_msoa_lad.csv")

lsoa11_msoa11 <- unique(all_11_lookup[, c("lsoa11cd", "msoa11cd")])

oa11_msoa11 <- unique(all_11_lookup[, c("oa11cd", "msoa11cd")])


  ### 1.2. narrowing down to required years
population <- population[year %in% 2010:2011, ]

deaths <- deaths[year == 2011, ]

births <- births[year == 2011, ]


  ### 1.3. aggregate to msoa11
deaths <- aggregate_geographies_2(deaths, lsoa11_msoa11, 
                                geog_from_data = "lsoa11cd", geog_from_lookup = "lsoa11cd",
                                geog_to_lookup = "msoa11cd", count_names = "deaths")


births <- aggregate_geographies_2(births, oa11_msoa11, 
                                    geog_from_data = "OA11CD", geog_from_lookup = "oa11cd",
                                    geog_to_lookup = "msoa11cd", count_names = "births")



## 2. net flow for all cohorts aside from the first and last 

  ### 2.1. small bits of cleaning up
population[age > 85, age := 85]

population <- population[, .(population = sum(population)),
                         by = .(sex, age, year, msoa11cd)]

population[, cohort := year - age]
deaths[, cohort := year - age]

  ### 2.2. getting the change in population between cohorts
population_prev <- copy(population)

population_prev[, year := year + 1]

population_prev <- population_prev[, -"age"]
colnames(population_prev)[4] <- "population_last_year"

setkey(population, "sex","year", "msoa11cd", "cohort")
setkey(population_prev, "sex","year", "msoa11cd", "cohort")

flows <- population_prev[population]

flows <- flows[!is.na(population_last_year), ]
flows[!is.na(population_last_year), gross_flows := population - population_last_year]

  ### 2.3. accounting for deaths, getting net migration
setkey(deaths, "msoa11cd", "year", "age", "sex", "cohort")
setkey(flows, "msoa11cd", "year", "age", "sex", "cohort")

flows <- deaths[flows]

flows[, gross_flows := gross_flows + deaths]


## 3. net flow for starting cohort (TO DO - again, I've copied and pasted code. Make a function that does this operation.)
births[, age := 0]
births[, cohort := year]

population_0 <- population[age == 0 & year == 2011, ]

setkey(population_0, "msoa11cd", "year", "age", "sex", "cohort")
setkey(births, "msoa11cd", "year", "age", "sex", "cohort")

flows_0 <- births[population_0]

flows_0[is.na(births), births := 0]

setkey(flows_0, "msoa11cd", "year", "sex", "age", "cohort")
setkey(deaths,  "msoa11cd", "year", "sex", "age", "cohort")

flows_0 <- deaths[flows_0]

flows_0[, gross_flows := population - births + deaths]


## 4. net flow for end cohort

### 4.1. creating the dataset of the previous cohort of 84 and 85+, which in the following year will become 85+
population_end <- population[age %in% 84:85, ]

population_end_prev <- copy(population_end)

population_end_prev <- dcast(population_end_prev, msoa11cd + year + sex ~ age, value.var = "population")

colnames(population_end_prev)[4:5] <- c("population_prev_84", "population_prev_85")

population_end_prev[, year := year + 1]


### 4.2. joining the two datasets
population_end <- population_end[age == 85, ]

setkey(population_end, "msoa11cd", "year", "sex")
setkey(population_end_prev, "msoa11cd", "year", "sex")

flows_end <- population_end_prev[population_end]
flows_end <- flows_end[year == 2011, ]

flows_end
deaths


### 4.3. joining deaths and calculating flows
setkey(flows_end, "msoa11cd", "year", "sex", "age", "cohort")
setkey(deaths, "msoa11cd", "year", "sex", "age", "cohort")


flows_end <- deaths[flows_end]

flows_end[, gross_flows := population - population_prev_85 - population_prev_84 + deaths]



## 5. joining together three datasets, making the final file

flows_0[, population_last_year := NA]
flows_0 <- flows_0[, c("lad23cd", "msoa11cd", "year", "age", "cohort", "sex", "births", "deaths", "population_last_year", "population", "gross_flows")]



flows_end[, population_last_year := population_prev_85 + population_prev_84]
flows_end[, births := NA]
flows_end <- flows_end[, c("lad23cd", "msoa11cd", "year", "age", "cohort", "sex", "births", "deaths", "population_last_year", "population", "gross_flows")]


flows <- flows[age != 85, ]
flows[, births := NA]
flows <- flows[, c("lad23cd", "msoa11cd", "year", "age", "cohort", "sex", "births", "deaths", "population_last_year", "population", "gross_flows")]

flows_all <- rbind(flows_0, flows, flows_end)


saveRDS(object = flows_all, 
        file = "input_data/intermediate/msoa11_flows.rds")



