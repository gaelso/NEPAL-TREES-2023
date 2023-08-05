
library(tidyverse)
library(sf)


file_path <- list.files("data", pattern = "ceo-Part", full.names = T) |>
  str_subset(pattern = "afterCorrection", negate = T)
file_path

file_path |> str_extract(pattern = "/[.*]-ART") 


ceo_plot <- map_dfr(file_path, function(x){
  tt <- read_csv(x) |> 
    mutate(
      filename = str_extract(x, pattern = "/.*-ART"),
      filename = str_remove(filename, pattern = "/"),
      filename = str_remove(filename, pattern = "-ART")
    ) |>
    select(filename, everything())
  tt
})

ceo_plot

length(unique(ceo_plot$plotid))

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
  rename(nb_disagreements = count)
dup_tab

write_csv(dup_tab, "results/disagreements-plotid.csv")

dup_plotid <- dup |>
  filter(count > 2) |>
  pull(plotid) |>
  unique()

length(dup_plotid)
dup_plotid

# names(ceo_plot_unique)

tt <- ceo_plot_unique |>
  filter(plotid %in% dup_plotid)



##
## FIND WHERE DISAGREEMENT HAPPENS #############################################
##

colnames <- ceo_plot_unique |>
  select(-plotid, -email, -starts_with("pl_"), -Notes, -filename) |>
  names()

## FOR TESTING ONLY
# x = dup_plotid[1]
# y = colnames[1]

## DOUBLE LOOP
## - for each plot, filter record
## - for each column, check if records distincts
disagreement <- map_dfr(dup_plotid, function(x){
  
  ## Filter 3 records for plotid "x"
  plot_data <- ceo_plot_unique |> filter(plotid == x)
  filename  <- unique(plot_data$filename) |> str_remove_all(pattern = "/ceo-|-ART") 
  
  ## For each column "y" check how many records
  col_disagreement <- map_dfr(colnames, function(y){
    
    n_values <- plot_data |> pull(y) |> unique() |> length()
    
    if (n_values != 1) {
      out <- tibble(filename = filename, plotid = x, disagreement_col = y)
    } else {
      out <- NULL
    }
    
    out
    
  })
})

disagreement

write_csv(disagreement, "results/disagreements_column-list.csv")



## 
## Create dataset with majority rule solved ####################################
##

## Add number of unique plot records  
ceo_plot_unique

vec_plotid <- unique(ceo_plot_unique$plotid)

ceo_plot_majo <- map_dfr(vec_plotid, function(x){
  
  tt <- ceo_plot_unique |> filter(plotid == x)
  
  n_distinct <- dup |> filter(plotid == x) |> pull(count)
  
  
  if (n_distinct == 1) {
    
    out <- 
    
  } else if (n_distinct == 2) {
    
    
  } else if (n_distinct == 3) {
    
    
  }
  
  
})
