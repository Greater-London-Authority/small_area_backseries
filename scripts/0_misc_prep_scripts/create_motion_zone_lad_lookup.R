library(dplyr)
library(readr)

# motion zones don't map to parent local authorities like MSOA and wards do.
# in many cases motion zones contain multiple local authorities
# we still want to assign an LA to each zone for now, 
# if only because the model is expecting the data in that form 
# we must ensure a 1-2-1 mapping

lookup_mz_lsoa21 <- readRDS("lookups/lsoa21cd_to_mz_updated.rds") |>
  rename(area_code = mz_code)

lookup_lsoa21_lad23 <- read_csv("lookups/lsoa21_lad23.csv") |>
  data.frame() |>
  rename(gss_code = lad23cd)

lookup_mz_lad23 <- lookup_mz_lsoa21 |>
  group_by(area_code) |>
  arrange(desc(weight)) |>
  ungroup() |>
  left_join(lookup_lsoa21_lad23, by = "lsoa21cd") |>
  select(-c(lsoa21cd, weight)) |>
  distinct(area_code, .keep_all = TRUE) 

write_csv(lookup_mz_lad23, "lookups/lookup_mz_gss_code.csv")
