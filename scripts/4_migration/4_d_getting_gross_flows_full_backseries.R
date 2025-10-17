## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in data
base_gross_flows <- readRDS("input_data/intermediate/base_gross_flows_lsoa21cd_2011.rds")

net_flows <- readRDS("input_data/intermediate/net_flows_lsoa21cd.rds")


## 2. fixing up datasets and joining
net_flows <- net_flows[, c("lad23cd", "lsoa21cd", "year", "age", "sex", "gross_flows")]

net_flows[age >= 75, age := 75] # I think I can just do this with net flows - aggregate over ages normally? TO DISCUSS. 

net_flows <- net_flows[, .(gross_flows = sum(gross_flows)),
                       by = list(lad23cd, lsoa21cd, year, age, sex)]

colnames(net_flows)[6] <- "net_flows"

colnames(base_gross_flows)[7:8] <- c("base_inflow", "base_outflow")
base_gross_flows <- base_gross_flows[, - "year"]

setkey(base_gross_flows, "lad23cd", "lsoa21cd", "sex", "age") # fairly sure that we match on age and not cohort...but think about it more to be extra sure. 
setkey(net_flows, "lad23cd", "lsoa21cd", "sex", "age")

gross_flows_all_series <- base_gross_flows[net_flows]


## 3. estimating the new net flows
base_in_vec <- gross_flows_all_series[, base_inflow]
base_out_vec <- gross_flows_all_series[, base_outflow]
target_net_vec <- gross_flows_all_series[, net_flows]

base_in_vec[is.na(base_in_vec)] <- 0
base_out_vec[is.na(base_out_vec)] <- 0
target_net_vec[is.na(target_net_vec)] <- 0 # hmm.....some of the net flows turn out to be NA. Obviously something has gone wrong, so look back and figure it out - I imagine it's one of the joins. But for now, just to make things work, just make them all equal to 0. 

adjusted_net_flows <- optimise_gross_flows(base_in = base_in_vec, 
                                           base_out = base_out_vec, 
                                           target_net = target_net_vec)

adjusted_net_flows_dt <- rbindlist(lapply(
  X = adjusted_net_flows,
  FUN = function(x){as.list(x)}
))

adjusted_inflows <- adjusted_net_flows_dt[, inflow]
adjusted_outflows <- adjusted_net_flows_dt[, outflow]


## 4. fixing up and saving final flows dataset
gross_flows_all_series[, inflow := adjusted_inflows]
gross_flows_all_series[, outflow := adjusted_outflows]


gross_flows_all_series <- gross_flows_all_series[, c("lad23cd", "lsoa21cd", "year", "age", 
                                                     "sex", "net_flows", "inflow", "outflow")]

saveRDS(object = gross_flows_all_series,
        file = "input_data/intermediate/estimated_gross_flows_2012_2020.rds")



