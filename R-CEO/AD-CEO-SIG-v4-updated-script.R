

library(tidyverse)
library(sf) ## If you want spatial vector data analysis
library(kableExtra)

user_input <- tibble(
  date_dl = "2023-08-24" ## Data of file download
)

dir.create("results", showWarnings = F)


## GS comments: 
## This script is based on ART-TREES-Activity_script_v4.R
## Parts of this script are handled in AD-CEO-disagreements.R
## The latest script deals in particular with duplicates and output a table at plot level with one row per 'plotid'
## Disagreements are handled by: 
##  1. Arbitrarily keep first record of a plot when there are 34 disagreements. These plots are supposed to be solved manually and once
##     this is done there shouldn't be any plot with this issue
##  2. Solve with majority rule in case 2 operators have the same info and the third has at least one difference.
##  3. Merge emails and notes and otherwise keep only one row when all 3 operators agree.


## This script adds to the data the columns added in ART-TREES-Activity_script_v4.R



################################################################################
# Import Necessary Data
################################################################################

# ------------------------------------------------------------------------------
# install necessary packages
# ------------------------------------------------------------------------------

## GS: Tidyverse already loaded and dplyr inc. in it.

# ------------------------------------------------------------------------------
# import in data & create dummy data
# ------------------------------------------------------------------------------

## GS comment:
## Here not sure what the GEE files contains that is not already in CEO. Ignored for now.
## Raw data from CEO are pre-cooked in AD-CEO-disagreements.R
## Loading here the output of that script

path_data <- list.files("results", pattern = paste0(user_input$date_dl, ".+CONCAT-MAJO"), full.names = T)

ceo_plot_init <- read_csv(path_data)



# ------------------------------------------------------------------------------
# check for data problems
# ------------------------------------------------------------------------------

# create a single function that checks for:
#  - if samples within plots are true duplicates (CEO main and QAQC)
#  - if any plotid/lat/lon combos were duplicated (CEO main and Q/QC (if separate))
#  - if plots between assessors are true duplicates (CEO QAQC)
# this function gives you a list of lists of the plotids relevant to each problem type
# and you will need to extract whichever list of plotids you are interested in

# (for the CEO main and CEO QAQC data (if it comes as two separate files) 
# you would extract duplicated plotid/lat/lon combos)

# (for CEO QAQC data (if it comes in one file), you would not want to extract 
# duplicated plotid/lat/lon combos because the data will (and should) 
# have duplicates of these)

## GS comment: 
## For Nepal ART-TREES CEO, I understand there is no QAQC separate file.
## Most of the original function is already included in the disagreement script.
## (Not sure what the check for email duplicates is used for)


################################################################################
# Write a Function to Analyze Data
################################################################################

## GS comment:
## Key parts of the function here without all that was already done in the disagreement analysis script

#################################
# Data Cleaning
#################################

# Strata Areas data ##########################################################

## GS: Commented out in the script V4

# GEE data ###################################################################

## GS: commented out in the script v4

# CEO data ###################################################################

# this was taken care of in functions above

# merge CEO QAQC and main data sets ##########################################

# deal with the fact that each plot has multiple sample points with their own 
# unique lat/lon

# create new lists of lats/lons that represent the actual centers of the plots

## GS comment: 
## Addressed in the disagreement script when simplifying from observation point to sample plot level.

## GS comment: 
## select() here used to position plot ID and coords as first columns.
## everything() can be used here to avoid having the mention explicitly each column name.
## WARNING: readr::read_csv() keeps original column names with spaces and surround them 
##          with backticks (`) instead of read.csv() which replace spaces with dots (.)

## GS Comment 2:
## The below code can all be grouped in one giant mutate() call is now split between
## several mutate() calls to keep the original structure of SIG v4 script.
names(ceo_plot_init)

ceo_plot <- ceo_plot_init |>
  mutate(plotid_unique = paste0(plotid, "a")) |>
  rename(lon_CEO = meanlon, lat_CEO = meanlat) |>
  select(
    plotid_unique,
    lon_CEO,
    lat_CEO,
    
    ## GS comment: 
    ## these are created during the QAQC and has been replaced 
    ## Best way to retrieve info is to look at the email.
    ## An additional column can be created in the disagreement script to make it more clear how many plots have 0, 1, or 2 disagreements
    
    # plot_differences_between_assessors__CEOqual,
    # sample_differences_within_plot__CEOqual,
    # sample_differences_within_plot__CEOqualduplicates,
    # plot_duplicates__CEOqualduplicates,
    # plot_duplicates__CEOqual,
    
    # sample_differences_within_plot__CEO,
    # plot_duplicates__CEO,
    everything()
  ) |>
  
  ##
  ## Standardize column names
  ##
  mutate(
    t0_type_forest = `Permanent or Secondary Forest 1991-2011`,
    t0_type_secondaryforest = `Type of secondary forest established (1991-2011)`,
    t0_yr_secondaryforest_establ = `Forest establishment year 1991-2011`,
    t0_type = `Forest in 2011/2012`,
    t0_numbertrees = `Number of tree covered samples (2011/2012)`,
    t0_type_nonforest = `Non-forest land use type in 2011/2012`,  
    
    t1_type = `Forest in 2016/2017`,
    t1_numbertrees = `Number of tree covered samples (2016/2017)`,
    t1_type_nonforest = `Non-forest land use type in 2016/2017`,
    t1_yr_loss = `Year of forest loss (2012-2016)`,
    t1_yr_gain = `Year of forest gain (2012-2016)`, 
    t1_type_secondaryforest = `Type of secondary forest established (2012-2016)`,
    t1_change = `Is this answer different from the label in 2011/2012`,
    
    t2_type = `Forest in 2021`,
    t2_numbertrees = `Number of tree covered samples (2021)`,
    t2_type_nonforest = `Non-forest land use type in 2021`,
    t2_yr_loss = `Year of forest loss (2017-2021)`,
    t2_yr_gain = `Year of forest gain (2017-2021)`,
    t2_type_secondaryforest = `Type of secondary forest established (2017-2021)`,
    t2_change = `Is this answer different from the label in 2016/2017`,
    
    t3_type = NA,
    t3_numbertrees = NA,
    t3_type_nonforest = NA,
    t3_yr_loss = NA,
    t3_yr_gain = NA,
    t3_type_secondaryforest = NA,
    t3_change = NA
  ) |>
  
  # rename all the column values to make them more easily readable/descriptive
  ## GS comment:
  ## {dplyr} introduces case_when() to make chains of ifelse() more elegant
  mutate(
    ## t0 ----------------------------------------------------------------------
    ## GS comment: 
    ## Logical error in the original code "Sec. For. 1991-2011 (...)" already take all matching records to 'secondary forest' leaving no possibility
    ## to have "Sec. For. 1991-2011 (...)" and year establishment <= 1983 assigned to 'permanent forest'
    ## After check no consequence of this error as there probably was no afforestation recorded before 1983
    t0_type_forest = case_when(
      t0_type_forest == "Permanent Forest 1991-2011(>10% CC with no permanent conversion to NF)" ~ 'permanent forest',
      t0_type_forest == "Non-forest in 2011 (consistently non-forest or lost by end of period)" ~ 'non forest',
      t0_type_forest == "Secondary Forest 1991-2011 (natural forest or planted forest established within this period)" &
        t0_yr_secondaryforest_establ > 1983 ~ 'secondary forest',
      t0_type_forest == "Secondary Forest 1991-2011 (natural forest or planted forest established within this period)" & 
        t0_yr_secondaryforest_establ <= 1983 ~  'permanent forest',
      TRUE ~ NA_character_
    ),
    t0_type_secondaryforest = case_when(
      t0_type_secondaryforest == "natural forest or planted native mixed"  ~ 'natural forest',
      t0_type_secondaryforest == "plantation or planted commercial forest" ~ 'plantation forest',
      t0_type_secondaryforest == "tree-shaded-cropland / silvopasture"     ~ 'shaded cropland',
      TRUE ~ NA_character_
    ),
    t0_type = case_when(
      t0_type == "Yes (>10% CC and at least 0.5 ha)" ~ 'forest',
      t0_type == "No"                                ~ 'non forest',
      TRUE ~ NA_character_
    ),
    t0_type_nonforest = case_when(
      t0_type_nonforest == "Grasslands"                          ~ 'grassland',
      t0_type_nonforest == "Unshaded Cropland (TCC 10% or less)" ~ 'unshaded cropland',
      t0_type_nonforest == "Other Land"                          ~ 'other land',
      t0_type_nonforest == "Settlements"                         ~ 'settlements',
      TRUE ~ NA_character_
    ),
    
    ## t1 ----------------------------------------------------------------------
    t1_type = case_when(
      t1_type == "Yes (>10% CC and at least 0.5 ha)" ~ 'forest',
      t1_type == "No"                                ~ 'non forest',
      TRUE ~ NA_character_
    ),
    t1_type_secondaryforest = case_when(
      t1_type_secondaryforest == "natural forest or planted native mixed"  ~ 'natural forest',
      t1_type_secondaryforest == "plantation or planted commercial forest" ~ 'plantation forest',
      t1_type_secondaryforest == "tree-shaded-cropland / silvopasture"     ~ 'shaded cropland',
      TRUE ~ NA_character_
    ),
    t1_type_nonforest = case_when(
      t1_type_nonforest == "Grasslands"                          ~ 'grassland',
      t1_type_nonforest == "Unshaded Cropland (TCC 10% or less)" ~ 'unshaded cropland',
      t1_type_nonforest == "Other Land"                          ~ 'other land',
      t1_type_nonforest == "Settlements"                         ~ 'settlements',
      TRUE ~ NA_character_
    ),
    
    ## t2 ----------------------------------------------------------------------
    t2_type = case_when(
      t2_type == "Yes (>10% CC and at least 0.5 ha)" ~ 'forest',
      t2_type == "No"                                ~ 'non forest',
      TRUE ~ NA_character_
    ),
    t2_type_secondaryforest = case_when(
      t2_type_secondaryforest == "natural forest or planted native mixed"  ~ 'natural forest',
      t2_type_secondaryforest == "plantation or planted commercial forest" ~ 'plantation forest',
      t2_type_secondaryforest == "tree-shaded-cropland / silvopasture"     ~ 'shaded cropland',
      TRUE ~ NA_character_
    ),
    t2_type_nonforest = case_when(
      t2_type_nonforest == "Grasslands"                          ~ 'grassland',
      t2_type_nonforest == "Unshaded Cropland (TCC 10% or less)" ~ 'unshaded cropland',
      t2_type_nonforest == "Other Land"                          ~ 'other land',
      t2_type_nonforest == "Settlements"                         ~ 'settlements',
      TRUE ~ NA_character_
    ),
    
    ## t3 ----------------------------------------------------------------------
    t3_type = case_when(
      t3_type == "Yes (>10% CC and at least 0.5 ha)" ~ 'forest',
      t3_type == "No"                                ~ 'non forest',
      TRUE ~ NA_character_
    ),
    t3_type_secondaryforest = case_when(
      t3_type_secondaryforest == "natural forest or planted native mixed"  ~ 'natural forest',
      t3_type_secondaryforest == "plantation or planted commercial forest" ~ 'plantation forest',
      t3_type_secondaryforest == "tree-shaded-cropland / silvopasture"     ~ 'shaded cropland',
      TRUE ~ NA_character_
    ),
    t3_type_nonforest = case_when(
      t3_type_nonforest == "Grasslands"                          ~ 'grassland',
      t3_type_nonforest == "Unshaded Cropland (TCC 10% or less)" ~ 'unshaded cropland',
      t3_type_nonforest == "Other Land"                          ~ 'other land',
      t3_type_nonforest == "Settlements"                         ~ 'settlements',
      TRUE ~ NA_character_
    ),
    
  ) |>
  
  # check for discrepancies in the change QAQC column for each time period 
  # (the columns with "Was this answer different than the previous time period")
  mutate(
    t1_changeQA = case_when(
      t1_change == "No - consistent land cover of forest or non-forest" & t0_type == t1_type                                 ~ TRUE,
      t1_change == "Yes - forest was GAINED between 2011/2012 and 2014/2015" & t0_type == "non forest" & t1_type == "forest" ~ TRUE,
      t1_change == "Yes - forest was LOST between 2011/2012 and 2014/2015" & t0_type == "forest" & t1_type == "non forest"   ~ TRUE,
      is.na(t1_change) & is.na(t0_type) & is.na(t1_type) ~ NA,
      TRUE ~ FALSE
    ),
    t2_changeQA = case_when(
      t2_change == "No - consistent land cover of forest or non-forest" & t1_type == t2_type                            ~ TRUE,
      t2_change == "Yes - forest was GAINED between 2017/2018 and 2021" & t1_type == "non forest" & t2_type == "forest" ~ TRUE,
      t2_change == "Yes - forest was LOST between 2017/2018 and 2021" & t1_type == "forest" & t2_type == "non forest"   ~ TRUE,
      is.na(t2_change) & is.na(t1_type) & is.na(t2_type) ~ NA,
      TRUE ~ FALSE
    ),
    t3_changeQA = case_when(
      t3_change == "No - consistent land cover of forest or non-forest" & t2_type == t3_type                            ~ TRUE,
      t3_change == "Yes - forest was GAINED between 2017/2018 and 2021" & t2_type == "non forest" & t3_type == "forest" ~ TRUE,
      t3_change == "Yes - forest was LOST between 2017/2018 and 2021" & t2_type == "forest" & t3_type == "non forest"   ~ TRUE,
      is.na(t3_change) & is.na(t2_type) & is.na(t3_type) ~ NA,
      TRUE ~ FALSE
    )
    
  ) |>
  
  # create new columns to capture the land cover types and changes in them 
  # between time periods
  mutate(
    # Time 0 -------------------------------------------------------------------
    t0_type_final = case_when(
      t0_type_forest == 'permanent forest' ~ "permanent forest",
      t0_type_forest == 'secondary forest' ~ paste("secondary", t0_type_secondaryforest),
      t0_type_forest == 'non forest'       ~ t0_type_nonforest,
      TRUE ~ NA
    ),
    
    # Time 1 -------------------------------------------------------------------                                               
    t1_disturbance_type = case_when(
      t0_type_forest == "permanent forest" & 
        t0_type == 'forest' & 
        t1_type  == 'forest' & 
        t0_numbertrees >= t1_numbertrees & 
        t1_numbertrees != 0 ~ "forest degradation",
      t0_type == 'forest' & t1_type  == 'forest' ~ "stable forest",
      t0_type == 'non forest' & t1_type  == 'non forest' ~ "stable non forest",
      t0_type == 'non forest' & t1_type  == 'forest' ~ "forest gain",
      t0_type == 'forest' & t1_type  == 'non forest' ~ "forest loss",
      TRUE ~ NA_character_
    ),
    t1_type_final = case_when(
      t1_disturbance_type == 'stable forest' | t1_disturbance_type == 'forest degradation' ~ t0_type_final,
      t1_disturbance_type == 'forest loss' | t1_disturbance_type == 'stable non forest'    ~ t1_type_nonforest,
      t1_disturbance_type == 'forest gain' ~ paste("secondary", t1_type_secondaryforest),
      TRUE ~ NA_character_
    ),  
    t1_disturbance_type_subcat = case_when(
      t1_disturbance_type == 'forest degradation' ~ "forest degradation",
      t1_disturbance_type == 'stable forest'      ~ "stable forest",
      t1_disturbance_type == 'stable non forest'  ~ "stable non forest",
      t1_disturbance_type == 'forest loss' & t0_type_final  == 'permanent forest'            ~ "permanent forest loss",
      t1_disturbance_type == 'forest gain' & t1_type_final  == 'secondary natural forest'    ~ "natural secondary forest gain",
      t1_disturbance_type == 'forest loss' & t0_type_final  == 'secondary natural forest'    ~ "natural secondary forest loss",
      t1_disturbance_type == 'forest gain' & t1_type_final  == "secondary plantation forest" ~ "plantation forest gain",
      t1_disturbance_type == 'forest loss' & t0_type_final  == "secondary plantation forest" ~ "plantation forest loss",
      t1_disturbance_type == 'forest gain' & t1_type_final  == 'secondary shaded cropland'   ~ "shaded cropland gain",
      t1_disturbance_type == 'forest loss' & t0_type_final  == 'secondary shaded cropland'   ~ "shaded cropland loss",
      TRUE ~ NA_character_
    ),
    t1_canopyincrease = if_else(
      t1_disturbance_type == "stable forest" & t0_numbertrees <= t1_numbertrees,
      "canopy increase", NA_character_
    ),
    t1_forestloss_year = if_else(
      t1_disturbance_type == 'forest loss',
      t1_yr_loss, NA_real_
    ),
    t1_forestdegradation_year = if_else(
      t1_disturbance_type == 'forest degradation',
      t1_yr_loss, NA_real_
    ),
    t1_forestgain_year = if_else(
      t1_disturbance_type == 'forest gain',
      t1_yr_gain, NA_real_
    ),
    t1_percentcanopychange = case_when(
      t0_numbertrees > 0 & t1_numbertrees > 0    ~ ((t1_numbertrees - t0_numbertrees) * 11.1),
      is.na(t0_numbertrees) & t1_numbertrees > 0 ~ (t1_numbertrees * 11.1),
      t0_numbertrees > 0 & is.na(t1_numbertrees) ~ (-t0_numbertrees * 11.1),
      TRUE ~ NA_real_
    ),
    
    # Time 2 -------------------------------------------------------------------
    t2_disturbance_type = case_when(
      t1_type_final == "permanent forest" &
        t1_type == 'forest' & 
        t2_type  == 'forest' & 
        t1_numbertrees >= t2_numbertrees &
        t2_numbertrees != 0 ~  "forest degradation",
      t1_type == 'forest' & t2_type  == 'forest' ~  "stable forest",
      t1_type == 'non forest' & t2_type  == 'non forest' ~ "stable non forest",
      t1_type == 'non forest' & t2_type  == 'forest'     ~ "forest gain",
      t1_type == 'forest' & t2_type  == 'non forest'     ~ "forest loss",
      TRUE ~ NA_character_
    ),
    t2_type_final = case_when(
      t2_disturbance_type == 'stable forest' | t2_disturbance_type == 'forest degradation' ~ t1_type_final,
      t2_disturbance_type == 'forest loss' | t2_disturbance_type == 'stable non forest'    ~ t2_type_nonforest,
      t2_disturbance_type == 'forest gain' ~  paste("secondary", t2_type_secondaryforest),
      TRUE ~ NA_character_
    ),
    t2_disturbance_type_subcat = case_when(
      t2_disturbance_type == 'forest degradation' ~ "forest degradation",
      t2_disturbance_type == 'stable forest'      ~ "stable forest",
      t2_disturbance_type == 'stable non forest'  ~ "stable non forest",
      t2_disturbance_type == 'forest loss' & t1_type_final  == 'permanent forest'            ~ "permanent forest loss",
      t2_disturbance_type == 'forest gain' & t2_type_final  == 'secondary natural forest'    ~ "natural secondary forest gain",
      t2_disturbance_type == 'forest loss' & t1_type_final  == 'secondary natural forest'    ~ "natural secondary forest loss",
      t2_disturbance_type == 'forest gain' & t2_type_final  == "secondary plantation forest" ~ "plantation forest gain",
      t2_disturbance_type == 'forest loss' & t1_type_final  == "secondary plantation forest" ~ "plantation forest loss",
      t2_disturbance_type == 'forest gain' & t2_type_final  == 'secondary shaded cropland'   ~ "shaded cropland gain",
      t2_disturbance_type == 'forest loss' & t1_type_final  == 'secondary shaded cropland'   ~ "shaded cropland loss",
      TRUE ~ NA_character_
    ),
    t2_canopyincrease = if_else(
      t2_disturbance_type == "stable forest" & t1_numbertrees <= t2_numbertrees,
      "forest canopy increase", NA_character_
    ),
    t2_forestloss_year = if_else(
      t2_disturbance_type == 'forest loss',
      t2_yr_loss, NA_real_
    ),
    t2_forestdegradation_year = if_else(
      t2_disturbance_type == 'forest degradation',
      t2_yr_loss, NA_real_
    ),
    t2_forestgain_year = if_else(
      t2_disturbance_type == 'forest gain',
      t2_yr_gain, NA_real_
    ),
    t2_percentcanopychange = case_when(
      t1_numbertrees > 0 & t2_numbertrees > 0    ~  ((t2_numbertrees - t1_numbertrees) * 11.1),
      is.na(t1_numbertrees) & t2_numbertrees > 0 ~ (t2_numbertrees * 11.1),
      t1_numbertrees > 0 & is.na(t2_numbertrees) ~ (-t1_numbertrees * 11.1),
      TRUE ~ NA_real_
    ),
    
    # Time 3 ---------------------------------------------------------------------
    t3_disturbance_type = case_when(
      t2_type == 'forest' & t3_type  == 'forest'         ~ "stable forest",
      t2_type == 'non forest' & t3_type  == 'non forest' ~ "stable non forest",
      t2_type == 'non forest' & t3_type  == 'forest'     ~ "forest gain",
      t2_type == 'forest' & t3_type  == 'non forest'     ~ "forest loss",
      t0_type_forest == "permanent forest" & 
        t2_type == 'forest' & 
        t3_type  == 'forest' &
        t2_numbertrees >= t3_numbertrees &
        t3_numbertrees != 0 ~ "forest degradation",
      TRUE ~ NA_character_
    ),
    t3_type_final = case_when(
      t3_disturbance_type == 'stable forest' | t3_disturbance_type == 'forest degradation' ~ t2_type_final,
      t3_disturbance_type == 'forest loss' | t3_disturbance_type == 'stable non forest'    ~ t3_type_nonforest,
      t3_disturbance_type == 'forest gain' ~ paste("secondary", t3_type_secondaryforest),
      TRUE ~ NA_character_
    ),
    t3_disturbance_type_subcat = case_when(
      t3_disturbance_type == 'forest degradation' ~ "forest degradation",
      t3_disturbance_type == 'stable forest'      ~ "stable forest",
      t3_disturbance_type == 'stable non forest'  ~ "stable non forest",
      t3_disturbance_type == 'forest loss' & t2_type_final  == 'permanent forest'            ~ "permanent forest loss",
      t3_disturbance_type == 'forest gain' & t3_type_final  == 'secondary natural forest'    ~ "natural secondary forest gain",
      t3_disturbance_type == 'forest loss' & t2_type_final  == 'secondary natural forest'    ~ "natural secondary forest loss",
      t3_disturbance_type == 'forest gain' & t3_type_final  == "secondary plantation forest" ~ "plantation forest gain",
      t3_disturbance_type == 'forest loss' & t2_type_final  == "secondary plantation forest" ~ "plantation forest loss",
      t3_disturbance_type == 'forest gain' & t3_type_final  == 'secondary shaded cropland'   ~ "shaded cropland gain",
      t3_disturbance_type == 'forest loss' & t2_type_final  == 'secondary shaded cropland'   ~ "shaded cropland loss",
      TRUE ~ NA_character_
    ),
    t3_canopyincrease = if_else(
      t3_disturbance_type == "stable forest" & t2_numbertrees <= t3_numbertrees,
      "forest canopy increase", NA_character_
    ),
    t3_forestloss_year = if_else(
      t3_disturbance_type == 'forest loss',
      t3_yr_loss, NA_real_
    ),
    t3_forestdegradation_year = if_else(
      t3_disturbance_type == 'forest degradation',
      t3_yr_loss, NA_real_
    ),
    t3_forestgain_year = if_else(
      t3_disturbance_type == 'forest gain',
      t3_yr_gain, NA_real_
    ),
    t3_percentcanopychange = case_when(
      t2_numbertrees > 0 & t3_numbertrees > 0     ~ ((t3_numbertrees - t2_numbertrees) * 11.1),
      is.na(t2_numbertrees) & t3_numbertrees > 0  ~ (t3_numbertrees * 11.1),
      t2_numbertrees > 0 &  is.na(t3_numbertrees) ~ (-t2_numbertrees * 11.1),
      TRUE ~ NA_real_
    )
    
  )

## CHECKS
table(ceo_plot$t0_type_forest, useNA = "ifany")
table(ceo_plot$t0_type, useNA = "ifany")
table(ceo_plot$t1_type, useNA = "ifany")
table(ceo_plot$t2_type, useNA = "ifany")

table(ceo_plot$t0_type, ceo_plot$t1_type, useNA = "ifany")
table(ceo_plot$t1_type, ceo_plot$t2_type, useNA = "ifany")
table(ceo_plot$t2_type, ceo_plot$t3_type, useNA = "ifany")

## Checks QA:
table(ceo_plot$t1_changeQA, useNA = "ifany")
table(ceo_plot$t2_changeQA, useNA = "ifany")

t1_fail_changeQA <- ceo_plot |> filter(!t1_changeQA)
t2_fail_changeQA <- ceo_plot |> filter(!t2_changeQA)

if (nrow(t1_fail_changeQA) > 0) print(paste0("FAILS CHANGE QA PLOT IDs: ", paste0(pull(t1_fail_changeQA, plotid), collapse = " ")))
if (nrow(t2_fail_changeQA) > 0) print(paste0("FAILS CHANGE QA PLOT IDs: ", paste0(pull(t2_fail_changeQA, plotid), collapse = " ")))

if (nrow(t1_fail_changeQA) > 0) write_csv(t1_fail_changeQA, paste0("results/t1_fail_changeQA-", user_input$date_dl, ".csv"))
if (nrow(t2_fail_changeQA) > 0) write_csv(t2_fail_changeQA, paste0("results/t2_fail_changeQA-", user_input$date_dl, ".csv"))


# ----------------------------------------------------------------------------

FINALDATASET <- ceo_plot |>
  rename(
    LTstrata = pl_ltstrata,
    MTDDstrata = pl_mtddstrata,
    CODEDstrata = pl_codedstrata,
    CCDCSMAstrata = pl_ccdcsmastrata,
    GEEcombo_strata = pl_agreementchangestrata,
    GEEcombo_strata_readable = pl_readable
  ) |>
  
  # get ready to calculate agreement for each map with CEO data
  # make new columns with readable strata names for each model
  # DEG:1
  # LOSS:2
  # GAIN:3
  # Nonforest:4
  # Forest:5
  mutate(
    #Landtrendr
    LT_strata_readable = case_when(
      LTstrata == 5 ~ 'stable forest',
      LTstrata == 1 ~ 'forest degradation',
      LTstrata == 2 ~ 'forest loss',
      LTstrata == 4 ~ 'stable non forest',
      LTstrata == 3 ~ 'forest gain',
      TRUE ~ 'NotReviewed'
    ),
    #Machine Learning
    ML_strata_readable = case_when(
      MTDDstrata == 5 ~ 'stable forest',
      MTDDstrata == 1 ~ 'forest degradation',
      MTDDstrata == 2 ~ 'forest loss',
      MTDDstrata == 4 ~ 'stable non forest',
      MTDDstrata == 3 ~ 'forest gain',
      TRUE ~ 'NotReviewed'
    ),
    #CODED
    CODED_strata_readable = case_when(
      CODEDstrata == 5 ~ 'stable forest',
      CODEDstrata == 1 ~ 'forest degradation',
      CODEDstrata == 2 ~ 'forest loss',
      CODEDstrata == 4 ~ 'stable non forest',
      CODEDstrata == 3 ~ 'forest gain',
      TRUE ~ 'NotReviewed'
    ),
    #CCDC-SMA
    CS_strata_readable = case_when(
      CCDCSMAstrata == 5 ~ 'stable forest',
      CCDCSMAstrata == 1 ~ 'forest degradation',
      CCDCSMAstrata == 2 ~ 'forest loss',
      CCDCSMAstrata == 4 ~ 'stable non forest',
      CCDCSMAstrata == 3 ~ 'forest gain',
      TRUE ~ 'NotReviewed'
    ),
    # combined GEE agreement strata
    GEEcombo_strata_readable2 = case_when(
      GEEcombo_strata == 5 ~ 'stable forest',
      GEEcombo_strata == 1 ~ 'forest degradation',
      GEEcombo_strata == 2 ~ 'forest loss',
      GEEcombo_strata == 4 ~ 'stable non forest',
      GEEcombo_strata == 3 ~ 'forest gain',
      TRUE ~ 'NotReviewed'
    )
    
  ) |>
  
  # Prep for Analysis ---
  # Make agreement tables
  # make the agreement tables for each individual map
  # (named SMEagreement_XXX, XXX = model name)
  mutate(
    # Time 1 ---
    # LandTrendr
    t1_LTcompare = case_when(
      t1_disturbance_type == LT_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Agree',
      t1_disturbance_type != LT_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # Machine Learning
    t1_MLcompare = case_when(
      t1_disturbance_type == ML_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Agree',
      t1_disturbance_type != ML_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # CODED
    t1_CODEDcompare = case_when(
      t1_disturbance_type == CODED_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Agree',
      t1_disturbance_type != CODED_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # CCDC-SMA
    t1_CScompare = case_when(
      t1_disturbance_type == CS_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Agree',
      t1_disturbance_type != CS_strata_readable &
        !is.na(t1_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    #GEEcombo
    t1_GEEcombocompare = case_when(
      t1_disturbance_type == GEEcombo_strata_readable2 &
        !is.na(t1_disturbance_type) ~ 'Agree',
      t1_disturbance_type != GEEcombo_strata_readable2 &
        !is.na(t1_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # Time 2 ---
    # make the agreement tables for each individual map (named SMEagreement_XXX, XXX = map name)
    # LandTrendr
    t2_LTcompare = case_when(
      t2_disturbance_type == LT_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Agree',
      t2_disturbance_type != LT_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # Machine Learning
    t2_MLcompare = case_when(
      t2_disturbance_type == ML_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Agree',
      t2_disturbance_type != ML_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # CODED
    t2_CODEDcompare = case_when(
      t2_disturbance_type == CODED_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Agree',
      t2_disturbance_type != CODED_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # CCDC-SMA
    t2_CScompare = case_when(
      t2_disturbance_type == CS_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Agree',
      t2_disturbance_type != CS_strata_readable &
        !is.na(t2_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # GEEcombo
    t2_GEEcombocompare = case_when(
      t2_disturbance_type == GEEcombo_strata_readable2 &
        !is.na(t2_disturbance_type) ~ 'Agree',
      t2_disturbance_type != GEEcombo_strata_readable2 &
        !is.na(t2_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # Time 3 ---
    # make the agreement tables for each individual map (named SMEagreement_XXX, XXX = map name)
    # LandTrendr
    t3_LTcompare = case_when(
      t3_disturbance_type == LT_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Agree',
      t3_disturbance_type != LT_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # Machine Learning
    t3_MLcompare = case_when(
      t3_disturbance_type == ML_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Agree',
      t3_disturbance_type != ML_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # CODED
    t3_CODEDcompare = case_when(
      t3_disturbance_type == CODED_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Agree',
      t3_disturbance_type != CODED_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # CCDC-SMA
    t3_CScompare = case_when(
      t3_disturbance_type == CS_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Agree',
      t3_disturbance_type != CS_strata_readable &
        !is.na(t3_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    # GEEcombo combo
    t3_GEEcombocompare = case_when(
      t3_disturbance_type == GEEcombo_strata_readable2 &
        !is.na(t3_disturbance_type) ~ 'Agree',
      t3_disturbance_type != GEEcombo_strata_readable2 &
        !is.na(t3_disturbance_type) ~ 'Dis',
      TRUE ~ 'NotReviewed'
    ),
    
  ) |>
  
  ## GS comment: easier to show REDD+ activities as codes
  mutate(
    t1_disturbance_type_code = case_when(
      t1_disturbance_type == "forest degradation" ~ "DG",
      t1_disturbance_type == "forest loss"        ~ "DF",
      t1_disturbance_type == "forest gain"        ~ "FG",
      t1_disturbance_type == "stable forest"      ~ "SF",
      t1_disturbance_type == "stable non forest"  ~ "SNF",
      TRUE ~ NA_character_
    ),
    t2_disturbance_type_code = case_when(
      t2_disturbance_type == "forest degradation" ~ "DG",
      t2_disturbance_type == "forest loss"        ~ "DF",
      t2_disturbance_type == "forest gain"        ~ "FG",
      t2_disturbance_type == "stable forest"      ~ "SF",
      t2_disturbance_type == "stable non forest"  ~ "SNF",
      TRUE ~ NA_character_
    ),
    GEEcombo_strata_readable2_code = case_when(
      GEEcombo_strata_readable2 == "forest degradation" ~ "DG",
      GEEcombo_strata_readable2 == "forest loss"        ~ "DF",
      GEEcombo_strata_readable2 == "forest gain"        ~ "FG",
      GEEcombo_strata_readable2 == "stable forest"      ~ "SF",
      GEEcombo_strata_readable2 == "stable non forest"  ~ "SNF",
      TRUE ~ NA_character_
    )
  )

names(FINALDATASET)

write_csv(FINALDATASET, paste0("results/CompiledData_CEO_GEE-ART-TREES-", user_input$date_dl, ".csv"))


## GS comment: Following tables are not really informative, suggest to move to the confusion matrices
# t1_SMEagreement_LandTrendr <- table(FINALDATASET$t1_LTcompare) 
# t1_SMEagreement_MachineLearning <- table(FINALDATASET$t1_MLcompare)
# t1_SMEagreement_CODED <- table(FINALDATASET$t1_CODEDcompare)
# t1_SMEagreement_CCDCSMA <- table(FINALDATASET$t1_CScompare)
# t1_agreement_GEEcombo <- table(FINALDATASET$t1_GEEcombocompare)
# 
# t2_SMEagreement_LandTrendr <- table(FINALDATASET$t2_LTcompare)
# t2_SMEagreement_MachineLearning <- table(FINALDATASET$t2_MLcompare)
# t2_SMEagreement_CODED <- table(FINALDATASET$t2_CODEDcompare)
# t2_SMEagreement_CCDCSMA <- table(FINALDATASET$t2_CScompare)
# t2_agreement_GEEcombo <- table(FINALDATASET$t2_GEEcombocompare)
# 
# t3_SMEagreement_LandTrendr <- table(FINALDATASET$t3_LTcompare)
# t3_SMEagreement_MachineLearning <- table(FINALDATASET$t3_MLcompare)
# t3_SMEagreement_CODED <- table(FINALDATASET$t3_CODEDcompare)
# t3_SMEagreement_CCDCSMA <- table(FINALDATASET$t3_CScompare)
# t3_agreement_GEEcombo <- table(FINALDATASET$t3_GEEcombocompare)

# ----------------------------------------------------------------------------

#################################
# Analysis
#################################

# Agreement (confusion) matrix ###############################################
## Function to create a confusion matrix with accuracy measures
## Inputs: 
##    .df a dataframe, 
##    .col_ref the column name of the reference class, i.e. the visual interpretation, 
##    .col_map the column name of the map class
##    .dest    NULL or a folder where to save the confusion matrices

## FOR TESTING ONLY
# .df = FINALDATASET
# .col_ref = quo(t1_disturbance_type_code)
# .col_map = quo(GEEcombo_strata_readable2_code)

make_confmat <- function(.df, .col_ref, .col_map, .dest){
  
  .col_map <- enquo(.col_map)
  .col_ref <- enquo(.col_ref)
  
  mat <- table(.df[[ quo_name(.col_map) ]], .df[[ quo_name(.col_ref) ]])
  
  ## Check
  message("Square matrix: ",  nrow(mat) == ncol(mat))
  
  #pur_mat  <- as.matrix(mat)
  diag_val <- diag(mat)
  tot_ref  <- as.numeric(colSums(mat, na.rm = T))
  tot_map  <- as.numeric(rowSums(mat, na.rm = T))
  com_err  <- round((tot_map - diag_val) / tot_map * 100, 0)
  omi_err  <- round((tot_ref - diag_val) / tot_ref * 100, 0)
  usr_acc  <- round(diag_val / tot_map * 100, 0)
  prd_acc  <- round(diag_val / tot_ref * 100, 0)
  OA       <- round(sum(diag_val) / sum(tot_ref) * 100, 0)
  
  ## Create printable table
  mat2 <- mat |>
    rbind(tot_ref, omi_err) |>
    cbind(tot_map = c(tot_map, 0, 0), com_err = c(com_err, 0, 0)) |>
    as_tibble() |>
    mutate(
      map_dat = "Map data",
      map_cat = c("Forest loss", "Forest degradation", "Forest gain", "Stable Forest", "Stable non-forest", "Total Reference", "Omission error (%)")
      ) |>
    select(map_dat, map_cat, everything())
  
  kab <- mat2 |>
    mutate(
      tot_map = cell_spec(tot_map, color = ifelse(tot_map == 0, "white", "black")),
      com_err = cell_spec(com_err, color = ifelse(com_err == 0, "white", "black")),
      map_cat = cell_spec(map_cat, bold = T),
      map_dat = cell_spec(map_dat, bold = T, angle = -90)
    ) |>
    kable(
      col.names = c("", "", "Forest loss", "Forest degradation", "Forest gain", "Stable Forest", "Stable non-forest", "Total map", "Commission error (%)"), 
      caption = paste0("Confusion matrix for map: '", quo_name(.col_map), "', and reference: '", quo_name(.col_ref), "'."),
      align = "llrrrrrrr",
      escape = F
      ) |>
    kable_styling(full_width = F) |>
    add_header_above(c(" " = 2, "Reference data" = 5, " " = 2)) |>
    collapse_rows(columns = 1, valign = "middle", row_group_label_position = "") |>
    footnote(general = paste0("<b>Overall accuracy = ", OA, "%.</b>"), general_title = " ", escape = F)
  
  if (!is.null(.dest)) {
    
    write_csv(
      mat2, 
      paste0(.dest, "/confusion-matrix-", quo_name(.col_map), "-", quo_name(.col_ref), "-", user_input$date_dl, ".csv")
    )
    
    save_kable(
      kab, 
      paste0(.dest, "/confusion-matrix-", quo_name(.col_map), "-", quo_name(.col_ref), "-", user_input$date_dl, ".png")
    )
    
  }

  list(mat2 = mat2, OA = OA, kab)

  }

confmat_t1 <- make_confmat(
  .df = FINALDATASET, 
  .col_ref = t1_disturbance_type_code, 
  .col_map = GEEcombo_strata_readable2_code, 
  .dest = "results"
  )

confmat_t2 <- make_confmat(
  .df = FINALDATASET, 
  .col_ref = t2_disturbance_type_code, 
  .col_map = GEEcombo_strata_readable2_code, 
  .dest = "results"
)

# confmat_t3 <- make_confmat(
#   .df = FINALDATASET, 
#   .col_ref = t3_disturbance_type_code, 
#   .col_map = GEEcombo_strata_readable2_code, 
#   .dest = "results"
# )

## GS Comment:
## Following code replaced with make_confmat() function + FINALDATASET already saved
## Regarding issues, disagreements already saved as CSV as well as ChangeQA

##
## END OF SCRIPT
##
################################################################################
################################################################################
################################################################################
################################################################################


# Time 1 ---------------------------------------------------------------------
# 
# #cross tabulation of strata by disturbance type
# t1_table_GEEstrata_CEOdisttype <- table(FINALDATASET$GEEcombo_strata_readable2, FINALDATASET$t1_disturbance_type)
# write.csv(t1_table_GEEstrata_CEOdisttype, file = paste0(results_filepath, '.t1_confusionmatrix_GEEstrata_CEOdisttype-ART-TREES-', part, '.csv'), row.names = T)
# 
# # Time 2 ---------------------------------------------------------------------
# 
# #cross tabulation of strata by disturbance type
# t2_table_GEEstrata_CEOdisttype <- table(FINALDATASET$GEEcombo_strata_readable2, FINALDATASET$t2_disturbance_type)
# write.csv(t2_table_GEEstrata_CEOdisttype, file = paste0(results_filepath, 't2_confusionmatrix_GEEstrata_CEOdisttype-ART-TREES-', part, '.csv'), row.names = T)
# 
# # Time 3 ---------------------------------------------------------------------
# 
# #cross tabulation of strata by disturbance type
# t3_table_GEEstrata_CEOdisttype <- table(FINALDATASET$GEEcombo_strata_readable2, FINALDATASET$t3_disturbance_type)
# write.csv(t3_table_GEEstrata_CEOdisttype, file = paste0(results_filepath, 't3_confusionmatrix_GEEstrata_CEOdisttype-ART-TREES-', part, '.csv'), row.names = T)
# 
# # ----------------------------------------------------------------------------

# # export final dataset #######################################################
# 
# # final data set
# write.csv(FINALDATASET,
#           file = paste0(results_filepath, 'CompiledData_CEO_GEE-ART-TREES-', part, '.csv'),
#           row.names = F)
# 
# # export data problems #######################################################
# 
# # combine data issues into a single string
# dataproblems_string = paste("\nCEO QAQC data - plot differences between assessors:",
#                             as.character(paste(unique(dataCEO$plotid_unique[!is.na(dataCEO$plot_differences_between_assessors__CEOqual)]), collapse = ", ")),
#                             "\nCEO QAQC - sample differences within plot:",
#                             as.character(paste(unique(dataCEO$plotid_unique[!is.na(dataCEO$sample_differences_within_plot__CEOqual)]), collapse = ", ")),
#                             "\nIn time period 1, these plots did not have changes that agreed with the previous answer:",
#                             paste(notcorrectchanges_t1, collapse=', '),
#                             "\nChange type:",
#                             as.character(paste(dataCEO$t1_change[dataCEO$t1_changeQA == FALSE], collapse='\n')),
#                             "\nLand cover type in t0:",
#                             as.character(paste(dataCEO$t0_type[dataCEO$t1_changeQA == FALSE], collapse='\n')),
#                             "\nLand cover type in t1:",
#                             as.character(paste(dataCEO$t1_type[dataCEO$t1_changeQA == FALSE], collapse='\n')),
#                             "\nIn time period 2, these plots did not have changes that agreed with the previous answer:",
#                             paste(notcorrectchanges_t2, collapse=', '),
#                             "\nChange type:",
#                             as.character(paste(dataCEO$t2_change[dataCEO$t2_changeQA == FALSE], collapse='\n')),
#                             "\nLand cover type in t1:",
#                             as.character(paste(dataCEO$t1_type[dataCEO$t2_changeQA == FALSE], collapse='\n')),
#                             "\nLand cover type in t2:",
#                             as.character(paste(dataCEO$t2_type[dataCEO$t2_changeQA == FALSE], collapse='\n')),
#                             sep = "\n\n") 
# 
# # print(dataproblems_string)
# 
# # print list of data issues to a word document
# writeLines(dataproblems_string, paste0(results_filepath, 'CEO_data_problems_ART-TREES-', part, '.doc'))

#################################
# CONFUSION MATRIX STUFF I HAVE NOT MODIFIED YET
#################################

# TIME 1
# 
# # counts of answers
# table(FINALDATASET$t1_disturbance_type)
# table(FINALDATASET$strata)
# 
# # Agreement (confusion) matrix ###############################################
# 
# #cross tabulation of strata by disturbance type
# t1_table_strata_CEOdisturbancetype <- table(FINALDATASET$strata, FINALDATASET$t1_disturbance_type)
# 
# # write.csv(table_strata_CEODISTTYPE, file = 'Results/crosstable_strata_CEODISTTYPE.csv', row.names = T)
# 
# #create a column that has simplified map strata
# FINALDATASET$SimpleMapClass<-"fixMe"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable forest"]<-"SF"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable non forest"]<-"SNF"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest loss"]<-"Deforestation"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest degradation"]<-"Degradation"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest gain"]<-"Reforestation"
# 
# # create a column that has simplified CEO disturbance type
# # CEO strata should stay the same as the strata design originals for area estimation
# # (this is only for easy viewing)
# FINALDATASET$t1_SimpleCEOClass<-"fixMe"
# FINALDATASET$t1_SimpleCEOClass[FINALDATASET$t1_disturbance_type == "stable forest"]<-"SF"
# FINALDATASET$t1_SimpleCEOClass[FINALDATASET$t1_disturbance_type == "stable non forest"]<-"SNF"
# FINALDATASET$t1_SimpleCEOClass[FINALDATASET$t1_disturbance_type == "forest loss"] <-"Deforestation"
# FINALDATASET$t1_SimpleCEOClass[FINALDATASET$t1_disturbance_type == "forest degradation"]<-"Degradation"
# FINALDATASET$t1_SimpleCEOClass[FINALDATASET$t1_disturbance_type == "forest gain"]<-"Reforestation"
# 
# # table(FINALDATASET$DISTTYPE)
# # table(FINALDATASET$strata)
# # table(FINALDATASET$SimpleMapClass)
# # table(FINALDATASET$SimpleCEOClass)
# 
# # Confusion matrix: simplified map classes v.s. simplified CEO classes
# t1_agreement_simple<-table(FINALDATASET$SimpleMapClass, FINALDATASET$t1_SimpleCEOClass)
# names(dimnames(t1_agreement_simple)) <- c('Map','CEO')
# # write.csv(agreement, file = 'Results/Agreements_INCLUDINGQAQC_SAVE_3maps.csv', row.names = T)
# 
# # Confusion matrix: original map classes v.s. simplified CEO classes #########
# t1_agreement<-table(FINALDATASET$strata, FINALDATASET$t1_disturbance_type)
# # agreement <- agreement[c('DEG', 'LOSS', 'GAIN', 'ComboChange', 'Nonforest', 'Forest'),
# #                        c('Deforestation', 'Degradation', 'multi-disturbance', 'SF', 'SNF', 'Reforestation')]
# names(dimnames(t1_agreement)) <- c('Map','CEO')
# # agreement
# # write.csv(agreement, file = 'Results/Agreements_INCLUDINGQAQC_SAVE_3maps_origstrata_CLEANEDsimple.csv', row.names = T)
# 
# # Individual map comparison tables (confusion matrices) #########
# #################################################################
# t1_LT_Compare_Table <- table(FINALDATASET$LT_strata_readable, FINALDATASET$t1_SimpleCEOClass)
# # write.csv(SMEagreement_LandTrendr, file = 'Results/CEO_LandTrendr_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(LT_Compare_Table, file = 'Results/LT_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t1_ML_Compare_Table <- table(FINALDATASET$ML_strata_readable, FINALDATASET$t1_SimpleCEOClass)
# # write.csv(SMEagreement_MachineLearning, file = 'Results/CEO_MachineLearning_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(ML_Compare_Table, file = 'Results/ML_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t1_CODED_Compare_Table <- table(FINALDATASET$CODED_strata_readable, FINALDATASET$t1_SimpleCEOClass)
# # write.csv(SMEagreement_CODED, file = 'Results/CEO_CODED_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(CODED_Compare_Table, file = 'Results/CODED_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t1_CCDCSMA_Compare_Table <- table(FINALDATASET$CS_strata_readable, FINALDATASET$t1_SimpleCEOClass)
# # write.csv(SMEagreement_CCDCSMA, file = 'Results/CEO_CCDCSMA_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(CCDCSMA_Compare_Table, file = 'Results/CCDCSMA_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t1_GEEcombo_Compare_Table <- table(FINALDATASET$GEEcombo_strata_readable, FINALDATASET$t1_SimpleCEOClass)
# # write.csv(agreement_GEEcombo, file = 'Results/CEO_GEEcombo_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(GEEcombo_Compare_Table, file = 'Results/GEEcombo_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# 
#
# # TIME 2
# 
# # # counts of answers
# # table(FINALDATASET$DISTURBANCE)
# table(FINALDATASET$t2_disturbance_type)
# table(FINALDATASET$strata)
# 
# ################################
# ## agreement (confusion) matrix
# #################################
# 
# #cross tab of strata by disturbance type
# t2_table_strata_CEODISTTYPE <- table(FINALDATASET$strata, FINALDATASET$t2_disturbance_type)
# # write.csv(table_strata_CEODISTTYPE, file = 'Results/crosstable_strata_CEODISTTYPE.csv', row.names = T)
# 
# #create a column that has reclassified map labels to match simplified CEO ####
# # unique(FINALDATASET$DISTTYPE)
# # unique(FINALDATASET$strata)
# FINALDATASET$SimpleMapClass<-"fixMe"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable forest"]<-"SF"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable non forest"]<-"SNF"
# 
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest loss"]<-"Deforestation"
# # just LOSS
# 
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest degradation"]<-"Degradation"
# 
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest gain"]<-"Reforestation"
# 
# 
# #create a column that has simplified CEO ####
# # unique(FINALDATASET$DISTTYPE)
# # unique(FINALDATASET$strata)
# 
# ######CEO strata should stay the same as the strata design originals for area estimation
# #######this is only for easy viewing
# FINALDATASET$t2_SimpleCEOClass<-"fixMe"
# FINALDATASET$t2_SimpleCEOClass[FINALDATASET$t2_disturbance_type == "stable forest"]<-"SF"
# FINALDATASET$t2_SimpleCEOClass[FINALDATASET$t2_disturbance_type == "stable non forest"]<-"SNF"
# FINALDATASET$t2_SimpleCEOClass[FINALDATASET$t2_disturbance_type == "forest loss"] <-"Deforestation"
# FINALDATASET$t2_SimpleCEOClass[FINALDATASET$t2_disturbance_type == "forest degradation"]<-"Degradation"
# FINALDATASET$t2_SimpleCEOClass[FINALDATASET$t2_disturbance_type == "forest gain"]<-"Reforestation"
# 
# # table(FINALDATASET$DISTTYPE)
# # table(FINALDATASET$strata)
# # table(FINALDATASET$SimpleMapClass)
# # table(FINALDATASET$SimpleCEOClass)
# 
# # Confusion matrix: simplified map classes v.s. simplified CEO classes #######
# t2_agreement_simple<-table(FINALDATASET$SimpleMapClass, FINALDATASET$t2_SimpleCEOClass)
# names(dimnames(t2_agreement_simple)) <- c('Map','CEO')
# # agreement_simple
# # write.csv(agreement, file = 'Results/Agreements_INCLUDINGQAQC_SAVE_3maps.csv', row.names = T)
# 
# # Confusion matrix: original map classes v.s. simplified CEO classes #########
# t2_agreement<-table(FINALDATASET$strata, FINALDATASET$t2_disturbance_type)
# # agreement <- agreement[c('DEG', 'LOSS', 'GAIN', 'ComboChange', 'Nonforest', 'Forest'),
# #                        c('Deforestation', 'Degradation', 'multi-disturbance', 'SF', 'SNF', 'Reforestation')]
# names(dimnames(t2_agreement)) <- c('Map','CEO')
# # agreement
# # write.csv(agreement, file = 'Results/Agreements_INCLUDINGQAQC_SAVE_3maps_origstrata_CLEANEDsimple.csv', row.names = T)
# 
# # Individual map comparison tables (confusion matrices) #########
# #################################################################
# t2_LT_Compare_Table <- table(FINALDATASET$LT_strata_readable, FINALDATASET$t2_SimpleCEOClass)
# # write.csv(SMEagreement_LandTrendr, file = 'Results/CEO_LandTrendr_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(LT_Compare_Table, file = 'Results/LT_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t2_ML_Compare_Table <- table(FINALDATASET$ML_strata_readable, FINALDATASET$t2_SimpleCEOClass)
# # write.csv(SMEagreement_MachineLearning, file = 'Results/CEO_MachineLearning_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(ML_Compare_Table, file = 'Results/ML_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t2_CODED_Compare_Table <- table(FINALDATASET$CODED_strata_readable, FINALDATASET$t2_SimpleCEOClass)
# # write.csv(SMEagreement_CODED, file = 'Results/CEO_CODED_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(CODED_Compare_Table, file = 'Results/CODED_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t2_CCDCSMA_Compare_Table <- table(FINALDATASET$CS_strata_readable, FINALDATASET$t2_SimpleCEOClass)
# # write.csv(SMEagreement_CCDCSMA, file = 'Results/CEO_CCDCSMA_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(CCDCSMA_Compare_Table, file = 'Results/CCDCSMA_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t2_GEEcombo_Compare_Table <- table(FINALDATASET$GEEcombo_strata_readable, FINALDATASET$t2_SimpleCEOClass)
# # write.csv(agreement_GEEcombo, file = 'Results/CEO_GEEcombo_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(GEEcombo_Compare_Table, file = 'Results/GEEcombo_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# 
# 
# # TIME 3
# 
# # # counts of answers
# # table(FINALDATASET$DISTURBANCE)
# table(FINALDATASET$t3_disturbance_type)
# table(FINALDATASET$strata)
# 
# ################################
# ## agreement (confusion) matrix
# #################################
# 
# #cross tab of strata by disturbance type
# t3_table_strata_CEODISTTYPE <- table(FINALDATASET$strata, FINALDATASET$t3_disturbance_type)
# # write.csv(table_strata_CEODISTTYPE, file = 'Results/crosstable_strata_CEODISTTYPE.csv', row.names = T)
# 
# #create a column that has reclassified map labels to match simplified CEO ####
# # unique(FINALDATASET$DISTTYPE)
# # unique(FINALDATASET$strata)
# FINALDATASET$SimpleMapClass<-"fixMe"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable forest"]<-"SF"
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable non forest"]<-"SNF"
# 
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest loss"]<-"Deforestation"
# # just LOSS
# 
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest degradation"]<-"Degradation"
# 
# FINALDATASET$SimpleMapClass[FINALDATASET$strata == "forest gain"]<-"Reforestation"
# 
# 
# #create a column that has simplified CEO ####
# # unique(FINALDATASET$DISTTYPE)
# # unique(FINALDATASET$strata)
# 
# ######CEO strata should stay the same as the strata design originals for area estimation
# #######this is only for easy viewing
# FINALDATASET$t3_SimpleCEOClass<-"fixMe"
# FINALDATASET$t3_SimpleCEOClass[FINALDATASET$t3_disturbance_type == "stable forest"]<-"SF"
# FINALDATASET$t3_SimpleCEOClass[FINALDATASET$t3_disturbance_type == "stable non forest"]<-"SNF"
# FINALDATASET$t3_SimpleCEOClass[FINALDATASET$t3_disturbance_type == "forest loss"] <-"Deforestation"
# FINALDATASET$t3_SimpleCEOClass[FINALDATASET$t3_disturbance_type == "forest degradation"]<-"Degradation"
# FINALDATASET$t3_SimpleCEOClass[FINALDATASET$t3_disturbance_type == "forest gain"]<-"Reforestation"
# 
# # table(FINALDATASET$DISTTYPE)
# # table(FINALDATASET$strata)
# # table(FINALDATASET$SimpleMapClass)
# # table(FINALDATASET$SimpleCEOClass)
# 
# # Confusion matrix: simplified map classes v.s. simplified CEO classes #######
# t3_agreement_simple<-table(FINALDATASET$SimpleMapClass, FINALDATASET$t3_SimpleCEOClass)
# names(dimnames(t3_agreement_simple)) <- c('Map','CEO')
# # agreement_simple
# # write.csv(agreement, file = 'Results/Agreements_INCLUDINGQAQC_SAVE_3maps.csv', row.names = T)
# 
# # Confusion matrix: original map classes v.s. simplified CEO classes #########
# t3_agreement<-table(FINALDATASET$strata, FINALDATASET$t3_disturbance_type)
# # agreement <- agreement[c('DEG', 'LOSS', 'GAIN', 'ComboChange', 'Nonforest', 'Forest'),
# #                        c('Deforestation', 'Degradation', 'multi-disturbance', 'SF', 'SNF', 'Reforestation')]
# names(dimnames(t3_agreement)) <- c('Map','CEO')
# # agreement
# # write.csv(agreement, file = 'Results/Agreements_INCLUDINGQAQC_SAVE_3maps_origstrata_CLEANEDsimple.csv', row.names = T)
# 
# # Individual map comparison tables (confusion matrices) #########
# #################################################################
# t3_LT_Compare_Table <- table(FINALDATASET$LT_strata_readable, FINALDATASET$t3_SimpleCEOClass)
# # write.csv(SMEagreement_LandTrendr, file = 'Results/CEO_LandTrendr_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(LT_Compare_Table, file = 'Results/LT_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t3_ML_Compare_Table <- table(FINALDATASET$ML_strata_readable, FINALDATASET$t3_SimpleCEOClass)
# # write.csv(SMEagreement_MachineLearning, file = 'Results/CEO_MachineLearning_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(ML_Compare_Table, file = 'Results/ML_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t3_CODED_Compare_Table <- table(FINALDATASET$CODED_strata_readable, FINALDATASET$t3_SimpleCEOClass)
# # write.csv(SMEagreement_CODED, file = 'Results/CEO_CODED_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(CODED_Compare_Table, file = 'Results/CODED_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t3_CCDCSMA_Compare_Table <- table(FINALDATASET$CS_strata_readable, FINALDATASET$t3_SimpleCEOClass)
# # write.csv(SMEagreement_CCDCSMA, file = 'Results/CEO_CCDCSMA_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(CCDCSMA_Compare_Table, file = 'Results/CCDCSMA_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# t3_GEEcombo_Compare_Table <- table(FINALDATASET$GEEcombo_strata_readable, FINALDATASET$t3_SimpleCEOClass)
# # write.csv(agreement_GEEcombo, file = 'Results/CEO_GEEcombo_agreement_rates_SAVE_simple.csv', row.names = T)
# # write.csv(GEEcombo_Compare_Table, file = 'Results/GEEcombo_Compare_Table_SAVE_simple.csv', row.names = T)
# 
# 
# 



# outlst <- dataproblems_string
# # agreement_simple, agreement, # 1, 2
# #                SMEagreement_LandTrendr, LT_Compare_Table, # 3.1
# #                SMEagreement_MachineLearning, ML_Compare_Table, # 3.2
# #                SMEagreement_CODED, CODED_Compare_Table,
# #                SMEagreement_CCDCSMA, CCDCSMA_Compare_Table,
# #                agreement_GEEcombo, GEEcombo_Compare_Table,  # 3.5
# #                FINALDATASET)  # 4
# return(outlst)
# # outlst
# 
# }
# 
# analyze(dataCEO, 
#         dataGEE,
#         dataCEO_qual, 
#         dataGEE_qual,
#         strataAreasGEE)

