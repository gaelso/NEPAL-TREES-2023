
library(sf)
library(tidyverse)
theme_set(theme_bw())

user_input <- tibble(
  date_dl = "2023-08-24" ## Data of file download
)

path_data <- list.files("results", pattern = paste0("CompiledData.+", user_input$date_dl), full.names = T)

ceo_plot <- read_csv(path_data)


##
## Planned LOSS ################################################################
##

ceo_loss <- ceo_plot |> filter(GEEcombo_strata_readable2_code == "DF")

table(ceo_loss$t1_disturbance_type_code, ceo_loss$t2_disturbance_type_code)

## >>> Most plots ended up in Degradation or stable non-forest

loss_SNF <-  ceo_loss |> filter(t1_disturbance_type_code == "SNF" | t2_disturbance_type_code == "SNF")
loss_SNF



