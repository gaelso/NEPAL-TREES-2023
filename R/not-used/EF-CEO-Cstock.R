
library(tidyverse)

file_path <- list.files("data", pattern = "ceo-NFI", full.names = T) |>
  str_subset(pattern = "afterCorrection", negate = T)
file_path

ceo_plot <- map_dfr(file_path, function(x){
  tt <- read_csv(x) |> mutate(filename = str_extract(x, pattern = "/.*-ART"))
  tt
})
ceo_plot

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

dup <- ceo_plot_unique |> 
  select(-email, -Notes) |> 
  distinct() |>
  group_by(filename, plotid) |>
  summarise(count = n()) 

table(dup$count, dup$filename)

dup_tab <- dup |>
  filter(count > 2) |>
  rename(nb_disagreements = count) |>
  mutate(filename = filename |> str_remove(pattern = "/") |> str_remove("-ART"))
dup_tab

dup_plotid_majo <- dup |>
  filter(count == 2) |>
  pull(plotid) |>
  unique()

map_dfr


