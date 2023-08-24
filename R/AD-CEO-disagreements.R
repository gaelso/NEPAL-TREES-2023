
## NEPAL ART-TREES Carbon accounting 
## Activity data Collect Earth Online data analysis
## https://app.collect.earth/collection?projectId=37002
## https://app.collect.earth/collection?projectId=37003
## https://app.collect.earth/collection?projectId=37004
## Gael Sola, FAO, gael.sola@fao.org, github.com/gaelso

## This script takes raw data from CEO and:
## 1. simplify from observation points to sample plot
## 2. identify the number of sample plots that have 1, 2 or 3 distinct records
## 3. simplify from 3 records per sample plot to 1 with the rules:
##    a. If 1 distinct records, regroup email and Notes and take unique values for all the other columns
##    b. If 2 distinct records, find the majority records and keep their values. Emails are grouped and annotated to track which record disagreed
##    c. IF 3 distinct records, keep the first record (arbitrary) for preliminary data analysis, noting these plots will be manually edited to solve the disagreements.


library(tidyverse)
library(sf)

user_input <- tibble(
  date_dl = "2023-08-18" ## Data of file download
)

dir.create("results", showWarnings = F)

## LOAD DATA
file_path <- list.files(path = "data", pattern = "ceo-Part", full.names = T) |> 
  str_subset(pattern = user_input$date_dl)
file_path

ceo_point <- map_dfr(file_path, function(x){
  tt <- read_csv(x, show_col_types = F) |> 
    mutate(
      filename = str_extract(x, pattern = "/.*-ART"),
      filename = str_remove(filename, pattern = "/"),
      filename = str_remove(filename, pattern = "-ART")
    ) |>
    select(filename, everything()) |>
    arrange(plotid, sampleid)
  tt
})

length(unique(ceo_point$plotid))



##
## Remove observation point level information and simplify data to sample level 
##

## Get center points lon-lat from observation points
plot_lonlat <- ceo_point |> 
  group_by(plotid) |>
  summarise(
    meanlat = median(lat),
    meanlon = median(lon)
  )


## Simplify the observation points to sample plots and add plot level lon-lat
ceo_plot <- ceo_point |>
  select(-sampleid, 
         -collection_time, 
         -flagged, 
         -analysis_duration, 
         -imagery_title, 
         -imagery_attributions, 
         -sample_geom,
         -lon,
         -lat) |>
  distinct(plotid, email, .keep_all = T) |>
  left_join(plot_lonlat, by = "plotid")

nrow(ceo_plot) / 3 ## Should be exactly 1576


##
## Find number of distinct records for each sample plot ########################
##

df_distinct <- ceo_plot |> 
  select(-email, -Notes) |> 
  distinct() |>
  group_by(filename, plotid) |>
  summarise(n_distinct = n(), .groups = "drop") 

table(df_distinct$n_distinct, df_distinct$filename)

## Save plot IDs with 3 disagreements
df_distinct |>
  filter(n_distinct == 3) |>
  write_csv(paste0("results/plotid-3disagreements-", user_input$date_dl, ".csv"))



##
## FIND WHERE DISAGREEMENTS HAPPEN #############################################
##

## CREATE FUNCTION TO FIND DISAGREEMENTS WITH DOUBLE LOOP
## - for each plot, filter 3 records
## - for each column, check if records are distinct

## Function parameters are 
##  + a dataframe to check
##  + a vector of plot IDs to check
##  + a vector of column names to check for duplicates
find_disagreements <- function(.data, .plotid, .colnames){
  
  message("Checking ", length(.plotid), " sample plots...")
  
  ## Create table of all combinations to check
  to_check <- expand_grid(plot_check = .plotid, col_check = .colnames)
  
  ## pmap() applies a function to each row of a df, the function inputs must correspond to the df column names.
  ## Within the function the df column names refer to each individual row.
  list_disa <- pmap(to_check, function(plot_check, col_check){
    
    ## For each plotid and column name, extract the three values and count the number of unique records
    value_check <- .data |>
      filter(plotid == plot_check) |>
      pull(col_check) |>
      unique()
    
    ## If more than 1 unique record, save the info as disagreement
    if (length(value_check) != 1) {
      out <- tibble(plotid = plot_check, disagreement_col = col_check)
    } else {
      out <- NULL
    }
    
    out
    
  }) |> list_rbind()
  
  message("...done!")
  list_disa
  
}


## Get columns for data collection
colnames <- ceo_plot |>
  select(-filename, -plotid, -email, -starts_with("pl_"), -Notes) |>
  names()

## Get plotid for 3 disagreements and columns of interest
plotid_3distinct <- df_distinct |> filter(n_distinct == 3) |> pull(plotid)

## Find disagreement's columns when 3 disagreements
list_disa3 <- find_disagreements(.data = ceo_plot, .plotid = plotid_3distinct, .colnames = colnames)
list_disa3

write_csv(list_disa3, paste0("results/list_disagreement_columns-", user_input$date_dl, ".csv"))



## 
## Create dataset with majority rule solved ####################################
##

## STEP 1: Get first record when 3 records disagree ----------------------------
ceo_plot_3disa <- ceo_plot |>
  filter(plotid %in% plotid_3distinct) |>
  distinct(plotid, .keep_all = TRUE)

## Check
length(unique(ceo_plot_3disa$plotid)) == nrow(ceo_plot_3disa)


## STEP 2: Solve 2 distinct records with majority rule -------------------------
plotid_2distinct <- df_distinct |> filter(n_distinct == 2) |> pull(plotid)

list_disa2 <- find_disagreements(.data = ceo_plot, .plotid = plotid_2distinct, .colnames = colnames)
list_disa2

## RUN LOOP over all  oplot with 2 distinct records
## - For each plot, find the first disagreement and find which email agree
## - 
ceo_plot_majo <- map_dfr(plotid_2distinct, function(x){
  
  ## Counter message
  pos <- length(plotid_2distinct[plotid_2distinct <= x])
  tot <- length(plotid_2distinct)
  if (round(pos/100)*100 == pos) message("Applying majority rule... ", pos, "/", tot)
  if (pos == tot) message("...Done ", pos, "/", tot)
  
  tt <- ceo_plot |> filter(plotid == x)
  
  col_disa <- list_disa2 |> filter(plotid == x) |> slice_head(n = 1) |> pull(disagreement_col)
  
  col_disa_count <- tt |>
    group_by(.data[[col_disa]]) |> ## See ?rlang::args_data_masking for tidy eval functions (.data[[varname_txt]] and {{ varname }})
    summarise(count = n())
  col_disa_count
  
  col_disa_majo <- col_disa_count |> filter(count == 2) |> pull(.data[[col_disa]])
  
  email_majo <- tt |> 
    filter(.data[[col_disa]] == col_disa_majo)|>
    pull(email)
  
  email_disa <- tt |>
    filter(.data[[col_disa]] != col_disa_majo)|>
    pull(email)
  
  out_email <- paste0("Agree: ", paste0(email_majo, collapse = " "), " - disagree: " , email_disa)
  out_notes <- paste0(tt$Notes, collapse = " - ")
  
  ## Output majority records only
  out <- tt |>
    filter(email %in% email_majo) |>
    mutate(email = out_email, Notes = out_notes) |>
    distinct()
  
})

ceo_plot_majo

## Check
length(unique(ceo_plot_majo$plotid)) == nrow(ceo_plot_majo)


## STEP 3: Keep only unique records if no disagreements ------------------------
plotid_nodisa <- df_distinct |> filter(n_distinct == 1) |> pull(plotid)

## RUN LOOP, combine email and notes
ceo_plot_nodisa <- map_dfr(plotid_nodisa, function(x){
  
  ## Show progress message
  pos <- length(plotid_nodisa[plotid_nodisa <= x])
  tot <- length(plotid_nodisa)
  if (round(pos/100)*100 == pos) message("Getting unique records... ", pos, "/", tot)
  if (pos == tot) message("...Done ", pos, "/", tot)
  
  ## Concatenate email and Notes to get 1 rows out of 3 identiocal records
  tt <- ceo_plot |> filter(plotid == x)

  out_email <- paste0(tt$email, collapse = " ")
  out_notes <- paste0(tt$Notes, collapse = " - ")
  
  out <- tt |>
    mutate(email = out_email, Notes = out_notes) |>
    distinct()
  
  out
  
})

ceo_plot_nodisa
length(unique(ceo_plot_nodisa$plotid)) == nrow(ceo_plot_nodisa)

## STEP 4: Combine all products ------------------------------------------------

ceo_plot_unique_final <- bind_rows(ceo_plot_nodisa, ceo_plot_majo, ceo_plot_3disa)
ceo_plot_unique_final

out_filename <- file_path[1] |> str_remove(".*-[1:9]-") |> str_remove(".csv")
out_filename

write_csv(ceo_plot_unique_final, paste0("results/ceo-", out_filename, "-CONCAT-MAJO.csv"))


