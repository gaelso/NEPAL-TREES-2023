
## R script translation of the file Nepal_LEAF_AD_tool_V4.xlsx.

## Gael Sola, FAO
## Nov 2023

library(sf)
library(readxl)
library(tidyverse)

version <- "V3_EmissionEstimate_20Oct"

ceo_filepath <- list.files(paste0("data/", version), pattern = "AD.+xlsx", full.names = T) |>
  str_subset(pattern = "\\~\\$", negate = T)
ceo_filepath

ceo_init <- readxl::read_excel(ceo_filepath, col_names = T, skip = 1)

names(ceo_init)


ceo <- ceo_init |>
  mutate(
    t2_disturbance_type_UPDATED = case_when(
      t2_disturbance_type == "forest degradation" & t1_numbertrees > 5 & t2_numbertrees > 5 ~ "stable forest",
      t2_disturbance_type == "forest degradation" & t1_numbertrees < 6 & t2_numbertrees < 6 ~ "stable forest",
      t2_disturbance_type == "forest degradation" & t1_numbertrees < 6 & t2_numbertrees > 5 ~ "forest gain",
      TRUE ~ t2_disturbance_type
    )
  )
  

algo <- ceo |>
  select(
    PLOTID, CS_strata_readable, CODED_strata_readable, LT_strata_readable, 
    ML_strata_readable, GEEcombo_strata_readable2, 
    t2_disturbance_type, t2_disturbance_type_UPDATED
    ) |>
  mutate(
    agreement_CD = if_else(CODED_strata_readable == t2_disturbance_type_UPDATED, 1, 0),
    agreement_ML = if_else(ML_strata_readable == t2_disturbance_type_UPDATED, 1, 0),
    agreement_CS = if_else(CS_strata_readable == t2_disturbance_type_UPDATED, 1, 0),
    agreement_LT = if_else(LT_strata_readable == t2_disturbance_type_UPDATED, 1, 0),
  )
algo 

table(algo$t2_disturbance_type)
table(algo$t2_disturbance_type_UPDATED)

round(sum(algo$agreement_CD)/nrow(algo) * 100)
round(sum(algo$agreement_ML)/nrow(algo) * 100)
round(sum(algo$agreement_CS)/nrow(algo) * 100)
round(sum(algo$agreement_LT)/nrow(algo) * 100)





