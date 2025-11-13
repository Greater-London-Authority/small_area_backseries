
## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

source("scripts/inputs.R")


## 1. reading in data
base_path_to_read <- paste0("input_data/intermediate/base_gross_flows_ward22cd_2011_", max_year, ".rds")

base_gross_flows <- readRDS(base_path_to_read)

net_flows <- readRDS("input_data/intermediate/net_flows_ward22cd.rds")

## 2. fixing up datasets and joining
net_flows <- net_flows[year >= min_year, c("ward22cd", "year", "age", "sex", "gross_flows")]

colnames(net_flows)[5] <- "net_flows"

colnames(base_gross_flows)[5:6] <- c("base_inflow", "base_outflow")

setkey(base_gross_flows, "ward22cd", "year", "sex", "age")
setkey(net_flows, "ward22cd", "year", "sex", "age")

gross_flows_all_series <- base_gross_flows[net_flows]

## 3. estimating the new net flows
base_in_vec <- gross_flows_all_series[, base_inflow]
base_out_vec <- gross_flows_all_series[, base_outflow]
target_net_vec <- gross_flows_all_series[, net_flows]

base_in_vec[is.na(base_in_vec)] <- 0
base_out_vec[is.na(base_out_vec)] <- 0
target_net_vec[is.na(target_net_vec)] <- 0

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


gross_flows_all_series <- gross_flows_all_series[, c("ward22cd", "year", "age", 
                                                     "sex", "net_flows", "inflow", "outflow")]

file_path <- paste0("input_data/intermediate/estimated_gross_flows_ward22_", min_year, "_", max_year, ".rds")

saveRDS(object = gross_flows_all_series,
        file = file_path)

rm(list = ls())
gc()


