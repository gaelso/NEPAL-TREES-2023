

library(readxl)
library(tidyverse)

file_path <- list.files("data", pattern = "Sheet1", full.names = T) |>
  str_subset(pattern = "20230803")

ceo <- readxl::read_xlsx(file_path)


## Get col names from row 1
names_ceo <- as.character(ceo[1,])
names(ceo) <- names_ceo

## Remove row 1 from data
ceo <- ceo |> filter(plotid_unique != "plotid_unique") 

ceo_gain <- ceo |>
  mutate(!is.na(t1_yr_gain)  )
  filter()

