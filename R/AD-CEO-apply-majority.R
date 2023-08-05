
library(tidyverse)

## Load files
file_path <- list.files("data", pattern = "ceo-Part", full.names = T) |>
  str_subset(pattern = "afterCorrection", negate = T)
file_path

ceo_plot <- map_dfr(file_path, function(x){
  tt <- read_csv(x) |> mutate(filename = str_extract(x, pattern = "/.*-ART"))
  tt
})

ceo_plot_unique <- ceo_plot |>
  select(-sampleid, 
         -collection_time, 
         -flagged, 
         -analysis_duration, 
         -imagery_title, 
         -imagery_attributions, 
         -sample_geom,
         -lon,
         -lat) |>
  distinct(plotid, email, .keep_all = T)

ceo_plot_unique


