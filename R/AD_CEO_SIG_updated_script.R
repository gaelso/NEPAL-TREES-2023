

library(tidyverse)
library(sf) ## If you want spatial vector data analysis

user_input <- tibble(
  date_dl = "2023-08-18" ## Data of file download
)

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

ceo_plot <- read_csv(path_data)



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
  
  ## For constistency with SIG script v4
  ceo_plot2 <- ceo_plot |>
    mutate(plotid_unique = paste0(plotid, "a")) |>
    rename(lon_CEO = meanlon, lat_CEO = meanlat)
  
  
  
  
  # t0_type_forest
  # t0_type_secondaryforest
  # t0_yr_secondaryforest_establ
  # 
  # t0_type
  # t0_numbertrees
  # t0_type_nonforest
  # 
  # t1_type
  # t1_numbertrees
  # t1_type_nonforest
  # t1_yr_loss
  # t1_yr_gain
  # t1_type_secondaryforest
  # t1_change
  # 
  # t2_type
  # t2_numbertrees
  # t2_type_nonforest
  # t2_yr_loss
  # t2_yr_gain
  # t2_type_secondaryforest
  # t2_change
  # 
  # t3_type
  # t3_numbertrees
  # t3_type_nonforest
  # t3_yr_loss
  # t3_yr_gain
  # t3_type_secondaryforest
  # t3_change
  
  # print(colnames(dataCEO))
  
  # rename all the columns to be more easily readable
  # remove unnecessary columns
  dataCEO <- dataCEO %>%  
    select(-email, 
           -sampleid, 
           -collection_time, 
           -flagged, 
           -analysis_duration, 
           -imagery_title, 
           -imagery_attributions, 
           -sample_geom,
           # -Notes,
           # -pl_sampleid, 
           -lon,
           -lat) %>%
    
    rename(plotid_original = plotid) %>% 
    
    select(plotid_unique,
           lon_CEO,
           lat_CEO,
           
           plot_differences_between_assessors__CEOqual,
           sample_differences_within_plot__CEOqual,
           sample_differences_within_plot__CEOqualduplicates,
           plot_duplicates__CEOqualduplicates,
           plot_duplicates__CEOqual,
           
           sample_differences_within_plot__CEO,
           plot_duplicates__CEO,
           
           plotid_original,
           pl_readable,
           pl_agreementchangestrata,
           pl_ltstrata:pl_ccdcsmastrata,
           Permanent.or.Secondary.Forest.1991.2011:Notes) %>%
    
    mutate(t0_type_forest = Permanent.or.Secondary.Forest.1991.2011,
           t0_type_secondaryforest = Type.of.secondary.forest.established..1991.2011.,
           t0_yr_secondaryforest_establ = Forest.establishment.year.1991.2011,
           t0_type = Forest.in.2011.2012,
           t0_numbertrees = Number.of.tree.covered.samples..2011.2012.,
           t0_type_nonforest = Non.forest.land.use.type.in.2011.2012,  
           
           t1_type = Forest.in.2016.2017,
           t1_numbertrees = Number.of.tree.covered.samples..2016.2017.,
           t1_type_nonforest = Non.forest.land.use.type.in.2016.2017,
           t1_yr_loss = Year.of.forest.loss..2012.2016.,
           t1_yr_gain = Year.of.forest.gain..2012.2016., 
           t1_type_secondaryforest = Type.of.secondary.forest.established..2012.2016.,
           t1_change = Is.this.answer.different.from.the.label.in.2011.2012,
           
           t2_type = Forest.in.2021,
           t2_numbertrees = Number.of.tree.covered.samples..2021.,
           t2_type_nonforest = Non.forest.land.use.type.in.2021,
           t2_yr_loss = Year.of.forest.loss..2017.2021.,
           t2_yr_gain = Year.of.forest.gain..2017.2021.,
           t2_type_secondaryforest = Type.of.secondary.forest.established..2017.2021.,
           t2_change = Is.this.answer.different.from.the.label.in.2016.2017,
           
           t3_type = NA,
           t3_numbertrees = NA,
           t3_type_nonforest = NA,
           t3_yr_loss = NA,
           t3_yr_gain = NA,
           t3_type_secondaryforest = NA,
           t3_change = NA) 
  
  # check that column names were renamed correctly
  # print(colnames(dataCEO))
  
  # print lists of the plotids with issues again
  
  # print("CEO main data - duplicate plots:")
  # print(dataCEO$plotid_unique[!is.na(dataCEO$plot_duplicates__CEO)])
  # 
  # print("CEO main data - sample differences within plots:")
  # print(dataCEO$plotid_unique[!is.na(dataCEO$sample_differences_within_plot__CEO)])
  
  print("CEO QAQC data - plot differences between assessors.")
  print(dataCEO$plotid_unique[!is.na(dataCEO$plot_differences_between_assessors__CEOqual)])
  
  print("CEO QAQC - sample differences within plot:")
  print(dataCEO$plotid_unique[!is.na(dataCEO$sample_differences_within_plot__CEOqual)])
  
  # print("CEO QAQC - duplicate plots:")
  # print(dataCEO$plotid_unique[!is.na(dataCEO$plot_duplicates__CEOqual)])
  # 
  # print("CEO QAQC duplicate data - sample differences within plot:")
  # print(dataCEO$plotid_unique[!is.na(dataCEO$sample_differences_within_plot__CEOqualduplicates)])
  # 
  # print("CEO QAQC duplicate data - duplicate plots:")
  # print(dataCEO$plotid_unique[!is.na(dataCEO$plot_duplicates__CEOqualduplicates)])
  
  # rename all the column values to make them more easily readable/descriptive
  
  # Time 0 ---------------------------------------------------------------------
  
  dataCEO$t0_type_forest <- ifelse(dataCEO$t0_type_forest == "Secondary Forest 1991-2011 (natural forest or planted forest established within this period)", 
                                   'secondary forest',
                                   ifelse(dataCEO$t0_type_forest == "Permanent Forest 1991-2011(>10% CC with no permanent conversion to NF)", 
                                          "permanent forest",
                                          ifelse(dataCEO$t0_type_forest == "Non-forest in 2011 (consistently non-forest or lost by end of period)", 
                                                 'non forest',
                                                 ifelse(dataCEO$t0_type_forest == "Secondary Forest 1991-2011 (natural forest or planted forest established within this period)" &
                                                          dataCEO$t0_yr_secondaryforest_establ <= 1983, 
                                                        'permanent forest',
                                                        NA))))
  
  dataCEO$t0_type_secondaryforest <- ifelse(dataCEO$t0_type_secondaryforest == "natural forest or planted native mixed", 'natural forest',
                                            ifelse(dataCEO$t0_type_secondaryforest == "plantation or planted commercial forest", "plantation forest",
                                                   ifelse(dataCEO$t0_type_secondaryforest == "tree-shaded-cropland / silvopasture", 'shaded cropland',
                                                          NA)))
  # t0_yr_secondaryforest_establ looks good
  
  dataCEO$t0_type <- ifelse(dataCEO$t0_type == "Yes (>10% CC and at least 0.5 ha)", 'forest',
                            ifelse(dataCEO$t0_type == "No", "non forest",
                                   NA))
  
  dataCEO$t0_type_nonforest = ifelse(dataCEO$t0_type_nonforest == "Grasslands", 'grassland',
                                     ifelse(dataCEO$t0_type_nonforest == "Unshaded Cropland (TCC 10% or less)", "unshaded cropland",
                                            ifelse(dataCEO$t0_type_nonforest == "Other Land", 'other land',
                                                   ifelse(dataCEO$t0_type_nonforest == "Settlements", 'settlements',
                                                          NA))))
  
  # t0_numbertrees looks good
  
  # Time 1 ---------------------------------------------------------------------
  
  dataCEO$t1_type <- ifelse(dataCEO$t1_type == "Yes (>10% CC and at least 0.5 ha)", 'forest',
                            ifelse(dataCEO$t1_type == "No", "non forest",
                                   NA))
  
  dataCEO$t1_type_secondaryforest <- ifelse(dataCEO$t1_type_secondaryforest == "natural forest or planted native mixed", 'natural forest',
                                            ifelse(dataCEO$t1_type_secondaryforest == "plantation or planted commercial forest", "plantation forest",
                                                   ifelse(dataCEO$t1_type_secondaryforest == "tree-shaded-cropland / silvopasture", 'shaded cropland',
                                                          NA)))
  
  dataCEO$t1_type_nonforest = ifelse(dataCEO$t1_type_nonforest == "Grasslands", 'grassland',
                                     ifelse(dataCEO$t1_type_nonforest == "Unshaded Cropland (TCC 10% or less)", "unshaded cropland",
                                            ifelse(dataCEO$t1_type_nonforest == "Other Land", 'other land',
                                                   ifelse(dataCEO$t1_type_nonforest == "Settlements", 'settlements',
                                                          NA))))
  
  # t1_numbertrees looks good
  # t1_yr_loss looks good
  # t1_yr_gain looks good
  
  # Time 2 --------------------------------------------------------------------- 
  
  dataCEO$t2_type <- ifelse(dataCEO$t2_type == "Yes (>10% CC and at least 0.5 ha)", 'forest',
                            ifelse(dataCEO$t2_type == "No", "non forest",
                                   NA))
  
  dataCEO$t2_type_secondaryforest <- ifelse(dataCEO$t2_type_secondaryforest == "natural forest or planted native mixed", 'natural forest',
                                            ifelse(dataCEO$t2_type_secondaryforest == "plantation or planted commercial forest", "plantation forest",
                                                   ifelse(dataCEO$t2_type_secondaryforest == "tree-shaded-cropland / silvopasture", 'shaded cropland',
                                                          NA)))
  
  dataCEO$t2_type_nonforest = ifelse(dataCEO$t2_type_nonforest == "Grasslands", 'grassland',
                                     ifelse(dataCEO$t2_type_nonforest == "Unshaded Cropland (TCC 10% or less)", "unshaded cropland",
                                            ifelse(dataCEO$t2_type_nonforest == "Other Land", 'other land',
                                                   ifelse(dataCEO$t2_type_nonforest == "Settlements", 'settlements',
                                                          NA))))
  
  # t2_numbertrees looks good
  # t2_yr_loss looks good
  # t2_yr_gain looks good
  
  # Time 3 ---------------------------------------------------------------------
  
  dataCEO$t3_type <- ifelse(dataCEO$t3_type == "Yes (>10% CC and at least 0.5 ha)", 'forest',
                            ifelse(dataCEO$t3_type == "No", "non forest",
                                   NA))
  
  dataCEO$t3_type_secondaryforest <- ifelse(dataCEO$t3_type_secondaryforest == "natural forest or planted native mixed", 'natural forest',
                                            ifelse(dataCEO$t3_type_secondaryforest == "plantation or planted commercial forest", "plantation forest",
                                                   ifelse(dataCEO$t3_type_secondaryforest == "tree-shaded-cropland / silvopasture", 'shaded cropland',
                                                          NA)))
  
  dataCEO$t3_type_nonforest = ifelse(dataCEO$t3_type_nonforest == "Grasslands", 'grassland',
                                     ifelse(dataCEO$t3_type_nonforest == "Unshaded Cropland (TCC 10% or less)", "unshaded cropland",
                                            ifelse(dataCEO$t3_type_nonforest == "Other Land", 'other land',
                                                   ifelse(dataCEO$t3_type_nonforest == "Settlements", 'settlements',
                                                          NA))))
  
  # t3_numbertrees looks good
  # t3_yr_loss looks good
  # t3_yr_gain looks good
  
  # ----------------------------------------------------------------------------
  
  # check for discrepancies in the change QAQC column for each time period 
  # (the columns with "Was this answer different than the previous time period")
  
  # Time 1 ---------------------------------------------------------------------
  
  dataCEO$t1_changeQA = ifelse(dataCEO$t1_change == "No - consistent land cover of forest or non-forest" & 
                                 dataCEO$t0_type == dataCEO$t1_type, 
                               TRUE,
                               ifelse(dataCEO$t1_change == "Yes - forest was GAINED between 2011/2012 and 2014/2015" & 
                                        dataCEO$t0_type == "non forest" &
                                        dataCEO$t1_type == "forest",
                                      TRUE,
                                      ifelse(dataCEO$t1_change == "Yes - forest was LOST between 2011/2012 and 2014/2015" & 
                                               dataCEO$t0_type == "forest" & 
                                               dataCEO$t1_type == "non forest",
                                             TRUE,
                                             ifelse(is.na(dataCEO$t1_change) & 
                                                      is.na(dataCEO$t0_type) &
                                                      is.na(dataCEO$t1_type), 
                                                    NA,
                                                    FALSE))))
  # notcorrectchanges_t1 = c()
  notcorrectchanges_t1 = dataCEO$plotid_unique[dataCEO$t1_changeQA == FALSE]
  # notcorrectchanges_t1 = append(notcorrectchanges_t1, notcorrectchange)
  if (length(notcorrectchanges_t1) != 0) {print(paste("In time period 1, these plots did not have changes that agreed with the previous answer:",
                                                   paste(notcorrectchanges_t1, collapse=', ')))}
  
  print("change type")
  print(dataCEO$t1_change[dataCEO$t1_changeQA == FALSE])
  print("land cover type in t0")
  print(dataCEO$t0_type[dataCEO$t1_changeQA == FALSE])
  print("land cover type in t1")
  print(dataCEO$t1_type[dataCEO$t1_changeQA == FALSE])
  
  # Time 2 ---------------------------------------------------------------------
  
  dataCEO$t2_changeQA <- ifelse(dataCEO$t2_change == "No - consistent land cover of forest or non-forest" &
                                  dataCEO$t1_type == dataCEO$t2_type,
                                TRUE,
                                ifelse(dataCEO$t2_change == "Yes - forest was GAINED between 2017/2018 and 2021" &
                                         dataCEO$t1_type == "non forest" &
                                         dataCEO$t2_type == "forest",
                                       TRUE,
                                       ifelse(dataCEO$t2_change == "Yes - forest was LOST between 2017/2018 and 2021" &
                                                dataCEO$t1_type == "forest" &
                                                dataCEO$t2_type == "non forest",
                                              TRUE,
                                              ifelse(is.na(dataCEO$t2_change) &
                                                       is.na(dataCEO$t1_type) &
                                                       is.na(dataCEO$t2_type),
                                                     NA,
                                                     FALSE))))
  # notcorrectchanges_t2 = c()
  notcorrectchanges_t2 = dataCEO$plotid_unique[dataCEO$t2_changeQA == FALSE]
  # notcorrectchanges_t2 = append(notcorrectchanges_t2, notcorrectchange)
  if (length(notcorrectchanges_t2) != 0) {print(paste("In time period 2, these plots did not have changes that agreed with the previous answer:",
                                                   paste(notcorrectchanges_t2, collapse=', ')))}
  
  print("change type")
  print(dataCEO$t2_change[dataCEO$t2_changeQA == FALSE])
  print("land cover type in t1")
  print(dataCEO$t1_type[dataCEO$t2_changeQA == FALSE])
  print("land cover type in t2")
  print(dataCEO$t2_type[dataCEO$t2_changeQA == FALSE])
  # unique(dataCEO$t1_type)
  # dataCEO$Forest.in.2014.2015[dataCEO$t2_changeQA == FALSE]
  # dataCEO$Forest.in.2017.2018[dataCEO$t2_changeQA == FALSE]
  
  # Time 3 ---------------------------------------------------------------------
  
  dataCEO$t3_changeQA = ifelse(dataCEO$t3_change == "No - consistent land cover of forest or non-forest" & 
                                 dataCEO$t2_type == dataCEO$t3_type, 
                               TRUE,
                               ifelse(dataCEO$t3_change == "Yes - forest was GAINED between 2017/2018 and 2021" & 
                                        dataCEO$t2_type == "non forest" &
                                        dataCEO$t3_type == "forest",
                                      TRUE,
                                      ifelse(dataCEO$t3_change == "Yes - forest was LOST between 2017/2018 and 2021" & 
                                               dataCEO$t2_type == "forest" & 
                                               dataCEO$t3_type == "non forest",
                                             TRUE,
                                             ifelse(is.na(dataCEO$t3_change) & 
                                                      is.na(dataCEO$t2_type) &
                                                      is.na(dataCEO$t3_type), 
                                                    NA,
                                                    FALSE))))
  # notcorrectchanges_t3 = c()
  # notcorrectchanges_t3 = dataCEO$plotid_unique[dataCEO$t3_changeQA == FALSE]
  # notcorrectchanges_t3 = append(notcorrectchanges_t3, notcorrectchange)
  # if (length(notcorrectchanges_t3) != 0) {print(paste("In time period 3, these plots did not have changes that agreed with the previous answer:",
  #                                                  paste(notcorrectchanges_t3, collapse=', ')))}
  # 
  # print("change type")
  # print(dataCEO$t3_change[dataCEO$t3_changeQA == FALSE])
  # print("land cover type in t2")
  # print(dataCEO$t2_type[dataCEO$t3_changeQA == FALSE])
  # print("land cover type in t3")
  # print(dataCEO$t3_type[dataCEO$t3_changeQA == FALSE])
  
  # ----------------------------------------------------------------------------
  
  # create new columns to capture the land cover types and changes in them 
  # between time periods
  
  # Time 0 ---------------------------------------------------------------------
  
  dataCEO$t0_type_final <- ifelse(dataCEO$t0_type_forest == 'permanent forest',
                                  "permanent forest",
                                  ifelse(dataCEO$t0_type_forest == 'secondary forest',
                                         paste("secondary", dataCEO$t0_type_secondaryforest),
                                         ifelse(dataCEO$t0_type_forest == 'non forest',
                                                dataCEO$t0_type_nonforest,
                                                NA)))
  
  # Time 1 ---------------------------------------------------------------------                                                   
  
  dataCEO$t1_disturbance_type <- ifelse(dataCEO$t0_type_forest == "permanent forest" &
                                          dataCEO$t0_type == 'forest' & 
                                          dataCEO$t1_type  == 'forest' &
                                          dataCEO$t0_numbertrees >= dataCEO$t1_numbertrees &
                                          dataCEO$t1_numbertrees != 0,
                                        "forest degradation",
                                        ifelse(dataCEO$t0_type == 'forest' & 
                                                 dataCEO$t1_type  == 'forest',
                                               "stable forest",
                                               ifelse(dataCEO$t0_type == 'non forest' & 
                                                        dataCEO$t1_type  == 'non forest',
                                                      "stable non forest",
                                                      ifelse(dataCEO$t0_type == 'non forest' & 
                                                               dataCEO$t1_type  == 'forest',
                                                             "forest gain",
                                                             ifelse(dataCEO$t0_type == 'forest' & 
                                                                      dataCEO$t1_type  == 'non forest',
                                                                    "forest loss",
                                                                    NA)))))
  
  dataCEO$t1_type_final <- ifelse(dataCEO$t1_disturbance_type == 'stable forest' |
                                    dataCEO$t1_disturbance_type == 'forest degradation',
                                  dataCEO$t0_type_final,
                                  ifelse(dataCEO$t1_disturbance_type == 'forest gain',
                                         paste("secondary", dataCEO$t1_type_secondaryforest),
                                         ifelse(dataCEO$t1_disturbance_type == 'forest loss' |
                                                  dataCEO$t1_disturbance_type == 'stable non forest',
                                                dataCEO$t1_type_nonforest,
                                                NA)))  
  
  dataCEO$t1_disturbance_type_subcat <- ifelse(dataCEO$t1_disturbance_type == 'forest degradation',
                                               "forest degradation",
                                               ifelse(dataCEO$t1_disturbance_type == 'stable forest',
                                                      "stable forest",
                                                      ifelse(dataCEO$t1_disturbance_type == 'stable non forest',
                                                             "stable non forest",
                                                             ifelse(dataCEO$t1_disturbance_type == 'forest loss' & 
                                                                      dataCEO$t0_type_final  == 'permanent forest',
                                                                    "permanent forest loss",
                                                                    ifelse(dataCEO$t1_disturbance_type == 'forest gain' & 
                                                                             dataCEO$t1_type_final  == 'secondary natural forest',
                                                                           "natural secondary forest gain",
                                                                           ifelse(dataCEO$t1_disturbance_type == 'forest loss' & 
                                                                                    dataCEO$t0_type_final  == 'secondary natural forest',
                                                                                  "natural secondary forest loss",
                                                                                  ifelse(dataCEO$t1_disturbance_type == 'forest gain' & 
                                                                                           dataCEO$t1_type_final  == "secondary plantation forest",
                                                                                         "plantation forest gain",
                                                                                         ifelse(dataCEO$t1_disturbance_type == 'forest loss' & 
                                                                                                  dataCEO$t0_type_final  == "secondary plantation forest",
                                                                                                "plantation forest loss",
                                                                                                ifelse(dataCEO$t1_disturbance_type == 'forest gain' & 
                                                                                                         dataCEO$t1_type_final  == 'secondary shaded cropland',
                                                                                                       "shaded cropland gain",
                                                                                                       ifelse(dataCEO$t1_disturbance_type == 'forest loss' & 
                                                                                                                dataCEO$t$t0_type_final  == 'secondary shaded cropland',
                                                                                                              "shaded cropland loss",
                                                                                                              NA))))))))))
  
  # dataCEO$t1_forestloss_type <- ifelse(dataCEO$t1_disturbance_type == 'forest loss' & 
  #                                              dataCEO$t1_type_forest  == 'permanent forest',
  #                                            "permanent forest loss",
  #                                            ifelse(dataCEO$t1_disturbance_type == 'forest loss' & 
  #                                                     dataCEO$t1_type_forest  == 'secondary forest',
  #                                                   "secondary forest loss",
  #                                                   NA))
  
  # dataCEO$t1_disturbanceYN <- ifelse(dataCEO$t1_disturbance_type == 'stable forest' |
  #                                          dataCEO$t1_disturbance_type == 'stable non forest',
  #                                        "no",
  #                                        ifelse(dataCEO$t1_disturbance_type == "forest gain" |
  #                                                 dataCEO$t1_disturbance_type == "forest loss"|
  #                                                 dataCEO$t1_disturbance_type == "forest degradation",
  #                                               "yes",
  #                                               NA))
  
  # dataCEO$t1_canopyincrease_type <- ifelse(dataCEO$t1_disturbance_type == "stable forest" &
  #                                             dataCEO$t0_numbertrees <= dataCEO$t1_numbertrees &
  #                                             dataCEO$t1_type_forest  == 'permanent forest',
  #                                           "permanent forest canopy increase",
  #                                           ifelse(dataCEO$t1_disturbance_type == "stable forest" &
  #                                                    dataCEO$t0_numbertrees <= dataCEO$t1_numbertrees &
  #                                                    dataCEO$t1_type_forest  == 'secondary forest',
  #                                                  "secondary forest canopy increase",
  #                                                  NA))
  
  dataCEO$t1_canopyincrease <- ifelse(dataCEO$t1_disturbance_type == "stable forest" &
                                        dataCEO$t0_numbertrees <= dataCEO$t1_numbertrees,
                                      "canopy increase",
                                      NA)
  
  dataCEO$t1_forestloss_year <- ifelse(dataCEO$t1_disturbance_type == 'forest loss',
                                       dataCEO$t1_yr_loss,
                                       NA)
  
  dataCEO$t1_forestdegradation_year <- ifelse(dataCEO$t1_disturbance_type == 'forest degradation',
                                              dataCEO$t1_yr_loss,
                                              NA)
  
  dataCEO$t1_forestgain_year <- ifelse(dataCEO$t1_disturbance_type == 'forest gain',
                                       dataCEO$t1_yr_gain,
                                       NA)
  
  # dataCEO$t1_canopyincrease_year <- ifelse(dataCEO$t1_canopyincrease_type == "permanent forest canopy increase"|
  #                                                  dataCEO$t1_canopyincrease_type == "secondary forest canopy increase",
  #                                            dataCEO$t1_yr_gain,
  #                                            NA)
  
  dataCEO$t1_percentcanopychange <- ifelse(dataCEO$t0_numbertrees > 0 &
                                             dataCEO$t1_numbertrees > 0,
                                           ((dataCEO$t1_numbertrees - dataCEO$t0_numbertrees) * 11.1),
                                           ifelse(is.na(dataCEO$t0_numbertrees) &
                                                    dataCEO$t1_numbertrees > 0,
                                                  (dataCEO$t1_numbertrees * 11.1),
                                                  ifelse(dataCEO$t0_numbertrees > 0 &
                                                           is.na(dataCEO$t1_numbertrees),
                                                         (-dataCEO$t0_numbertrees * 11.1),
                                                         NA)))
  
  # Time 2 ---------------------------------------------------------------------
  
  dataCEO$t2_disturbance_type <- ifelse(dataCEO$t1_type_final == "permanent forest" &
                                          dataCEO$t1_type == 'forest' & 
                                          dataCEO$t2_type  == 'forest' &
                                          dataCEO$t1_numbertrees >= dataCEO$t2_numbertrees &
                                          dataCEO$t2_numbertrees != 0,
                                        "forest degradation",
                                        ifelse(dataCEO$t1_type == 'forest' & 
                                                 dataCEO$t2_type  == 'forest',
                                               "stable forest",
                                               ifelse(dataCEO$t1_type == 'non forest' & 
                                                        dataCEO$t2_type  == 'non forest',
                                                      "stable non forest",
                                                      ifelse(dataCEO$t1_type == 'non forest' & 
                                                               dataCEO$t2_type  == 'forest',
                                                             "forest gain",
                                                             ifelse(dataCEO$t1_type == 'forest' & 
                                                                      dataCEO$t2_type  == 'non forest',
                                                                    "forest loss",
                                                                    NA)))))
  
  dataCEO$t2_type_final <- ifelse(dataCEO$t2_disturbance_type == 'stable forest' |
                                    dataCEO$t2_disturbance_type == 'forest degradation',
                                  dataCEO$t1_type_final,
                                  ifelse(dataCEO$t2_disturbance_type == 'forest gain',
                                         paste("secondary", dataCEO$t2_type_secondaryforest),
                                         ifelse(dataCEO$t2_disturbance_type == 'forest loss' |
                                                  dataCEO$t2_disturbance_type == 'stable non forest',
                                                dataCEO$t2_type_nonforest,
                                                NA)))  
  
  dataCEO$t2_disturbance_type_subcat <- ifelse(dataCEO$t2_disturbance_type == 'forest degradation',
                                               "forest degradation",
                                               ifelse(dataCEO$t2_disturbance_type == 'stable forest',
                                                      "stable forest",
                                                      ifelse(dataCEO$t2_disturbance_type == 'stable non forest',
                                                             "stable non forest",
                                                             ifelse(dataCEO$t2_disturbance_type == 'forest loss' & 
                                                                      dataCEO$t1_type_final  == 'permanent forest',
                                                                    "permanent forest loss",
                                                                    ifelse(dataCEO$t2_disturbance_type == 'forest gain' & 
                                                                             dataCEO$t2_type_final  == 'secondary natural forest',
                                                                           "natural secondary forest gain",
                                                                           ifelse(dataCEO$t2_disturbance_type == 'forest loss' & 
                                                                                    dataCEO$t1_type_final  == 'secondary natural forest',
                                                                                  "natural secondary forest loss",
                                                                                  ifelse(dataCEO$t2_disturbance_type == 'forest gain' & 
                                                                                           dataCEO$t2_type_final  == "secondary plantation forest",
                                                                                         "plantation forest gain",
                                                                                         ifelse(dataCEO$t2_disturbance_type == 'forest loss' & 
                                                                                                  dataCEO$t1_type_final  == "secondary plantation forest",
                                                                                                "plantation forest loss",
                                                                                                ifelse(dataCEO$t2_disturbance_type == 'forest gain' & 
                                                                                                         dataCEO$t2_type_final  == 'secondary shaded cropland',
                                                                                                       "shaded cropland gain",
                                                                                                       ifelse(dataCEO$t2_disturbance_type == 'forest loss' & 
                                                                                                                dataCEO$t$t1_type_final  == 'secondary shaded cropland',
                                                                                                              "shaded cropland loss",
                                                                                                              NA))))))))))
  
  dataCEO$t2_canopyincrease <- ifelse(dataCEO$t2_disturbance_type == "stable forest" &
                                        dataCEO$t1_numbertrees <= dataCEO$t2_numbertrees,
                                      "forest canopy increase",
                                      NA)
  
  dataCEO$t2_forestloss_year <- ifelse(dataCEO$t2_disturbance_type == 'forest loss',
                                       dataCEO$t2_yr_loss,
                                       NA)
  
  dataCEO$t2_forestdegradation_year <- ifelse(dataCEO$t2_disturbance_type == 'forest degradation',
                                              dataCEO$t2_yr_loss,
                                              NA)
  
  dataCEO$t2_forestgain_year <- ifelse(dataCEO$t2_disturbance_type == 'forest gain',
                                       dataCEO$t2_yr_gain,
                                       NA)
  
  dataCEO$t2_percentcanopychange <- ifelse(dataCEO$t1_numbertrees > 0 &
                                             dataCEO$t2_numbertrees > 0,
                                           ((dataCEO$t2_numbertrees - dataCEO$t1_numbertrees) * 11.1),
                                           ifelse(is.na(dataCEO$t1_numbertrees) &
                                                    dataCEO$t2_numbertrees > 0,
                                                  (dataCEO$t2_numbertrees * 11.1),
                                                  ifelse(dataCEO$t1_numbertrees > 0 &
                                                           is.na(dataCEO$t2_numbertrees),
                                                         (-dataCEO$t1_numbertrees * 11.1),
                                                         NA)))
  
  
  # Time 3 ---------------------------------------------------------------------
  
  dataCEO$t3_disturbance_type <- ifelse(dataCEO$t2_type == 'forest' & 
                                          dataCEO$t3_type  == 'forest',
                                        "stable forest",
                                        ifelse(dataCEO$t2_type == 'non forest' & 
                                                 dataCEO$t3_type  == 'non forest',
                                               "stable non forest",
                                               ifelse(dataCEO$t2_type == 'non forest' & 
                                                        dataCEO$t3_type  == 'forest',
                                                      "forest gain",
                                                      ifelse(dataCEO$t2_type == 'forest' & 
                                                               dataCEO$t3_type  == 'non forest',
                                                             "forest loss",
                                                             ifelse(dataCEO$t0_type_forest == "permanent forest" &
                                                                      dataCEO$t2_type == 'forest' & 
                                                                      dataCEO$t3_type  == 'forest' &
                                                                      dataCEO$t2_numbertrees >= dataCEO$t3_numbertrees &
                                                                      dataCEO$t3_numbertrees != 0,
                                                                    "forest degradation",
                                                                    NA)))))
  
  dataCEO$t3_type_final <- ifelse(dataCEO$t3_disturbance_type == 'stable forest' |
                                    dataCEO$t3_disturbance_type == 'forest degradation',
                                  dataCEO$t2_type_final,
                                  ifelse(dataCEO$t3_disturbance_type == 'forest gain',
                                         paste("secondary", dataCEO$t3_type_secondaryforest),
                                         ifelse(dataCEO$t3_disturbance_type == 'forest loss' |
                                                  dataCEO$t3_disturbance_type == 'stable non forest',
                                                dataCEO$t3_type_nonforest,
                                                NA)))  
  
  dataCEO$t3_disturbance_type_subcat <- ifelse(dataCEO$t3_disturbance_type == 'forest degradation',
                                               "forest degradation",
                                               ifelse(dataCEO$t3_disturbance_type == 'stable forest',
                                                      "stable forest",
                                                      ifelse(dataCEO$t3_disturbance_type == 'stable non forest',
                                                             "stable non forest",
                                                             ifelse(dataCEO$t3_disturbance_type == 'forest loss' & 
                                                                      dataCEO$t2_type_final  == 'permanent forest',
                                                                    "permanent forest loss",
                                                                    ifelse(dataCEO$t3_disturbance_type == 'forest gain' & 
                                                                             dataCEO$t3_type_final  == 'secondary natural forest',
                                                                           "natural secondary forest gain",
                                                                           ifelse(dataCEO$t3_disturbance_type == 'forest loss' & 
                                                                                    dataCEO$t2_type_final  == 'secondary natural forest',
                                                                                  "natural secondary forest loss",
                                                                                  ifelse(dataCEO$t3_disturbance_type == 'forest gain' & 
                                                                                           dataCEO$t3_type_final  == "secondary plantation forest",
                                                                                         "plantation forest gain",
                                                                                         ifelse(dataCEO$t3_disturbance_type == 'forest loss' & 
                                                                                                  dataCEO$t2_type_final  == "secondary plantation forest",
                                                                                                "plantation forest loss",
                                                                                                ifelse(dataCEO$t3_disturbance_type == 'forest gain' & 
                                                                                                         dataCEO$t3_type_final  == 'secondary shaded cropland',
                                                                                                       "shaded cropland gain",
                                                                                                       ifelse(dataCEO$t3_disturbance_type == 'forest loss' & 
                                                                                                                dataCEO$t$t2_type_final  == 'secondary shaded cropland',
                                                                                                              "shaded cropland loss",
                                                                                                              NA))))))))))
  
  dataCEO$t3_canopyincrease <- ifelse(dataCEO$t3_disturbance_type == "stable forest" &
                                        dataCEO$t2_numbertrees <= dataCEO$t3_numbertrees,
                                      "forest canopy increase",
                                      NA)
  
  dataCEO$t3_forestloss_year <- ifelse(dataCEO$t3_disturbance_type == 'forest loss',
                                       dataCEO$t3_yr_loss,
                                       NA)
  
  dataCEO$t3_forestdegradation_year <- ifelse(dataCEO$t3_disturbance_type == 'forest degradation',
                                              dataCEO$t3_yr_loss,
                                              NA)
  
  dataCEO$t3_forestgain_year <- ifelse(dataCEO$t3_disturbance_type == 'forest gain',
                                       dataCEO$t3_yr_gain,
                                       NA)
  
  dataCEO$t3_percentcanopychange <- ifelse(dataCEO$t2_numbertrees > 0 &
                                             dataCEO$t3_numbertrees > 0,
                                           ((dataCEO$t3_numbertrees - dataCEO$t2_numbertrees) * 11.1),
                                           ifelse(is.na(dataCEO$t2_numbertrees) &
                                                    dataCEO$t3_numbertrees > 0,
                                                  (dataCEO$t3_numbertrees * 11.1),
                                                  ifelse(dataCEO$t2_numbertrees > 0 &
                                                           is.na(dataCEO$t3_numbertrees),
                                                         (-dataCEO$t2_numbertrees * 11.1),
                                                         NA)))
  
  # write.csv(dataCEO, 
  #           file = './results/CEOdata_readyforanalysis.csv',
  #           row.names = F)
  
  # ----------------------------------------------------------------------------
  
  # Merge CEO and GEE data #####################################################
  
  # # merge data sets with CEO answers with strata info (from GEE)
  # 
  # # by plot ID
  # # FINALDATASET <- merge(dataCEO, dataGEE,
  # #                       by.x = c("plotid"), by.y = c("PLOTID"), all.x = F)
  # 
  # # by lat/lon
  # FINALDATASET <- merge(dataCEO, dataGEE,
  #                       by.x = c("lon_CEO","lat_CEO"),
  #                       by.y = c("lon_GEE","lat_GEE"),
  #                       all.x = F)
  # 
  # # final check to ensure there are no duplicates
  # FINALDATASET <- distinct(FINALDATASET)
  # 
  # # fill records with empty GEEcombo_strata with the pl_agreementchangestrata
  # FINALDATASET$GEEcombo_strata[FINALDATASET$GEEcombo_strata %>% is.na()] <-
  #   FINALDATASET$pl_agreementchangestrata[FINALDATASET$GEEcombo_strata %>% is.na()]
  # 
  # # Merge dataset with CEO answers with the map counts of the sample strata
  # FINALDATASET <- merge(FINALDATASET, strataAreasGEE,
  #                       by.x = c('GEEcombo_strata', 'GEEcombo_strata_readable'),
  #                       by.y = c('map_value', 'readable'),
  #                       all.x = F) %>%
  #   select(plotid_unique,
  #          LON,
  #          LAT,
  #          plotid_original:t3_percentcanopychange,
  #          GEEcombo_strata_readable,
  #          GEEcombo_strata,
  #          CODEDstrata:map_value_combinations
  #          )
  # colnames(FINALDATASET)
  # 
  # # ## fill records with empty disturbance type with the stable label from disturbance column
  # # FINALDATASET$DISTTYPE[FINALDATASET$DISTTYPE==NA] <-
  # #   FINALDATASET$disturbance[FINALDATASET$DISTTYPE==NA]
  # 
  # # write.csv(FINALDATASET, file = './results/CompiledData_CEO_GEE_readyforanalysis.csv', row.names = F)
  
  FINALDATASET = dataCEO %>% 
    rename(LTstrata = pl_ltstrata,
           MTDDstrata = pl_mtddstrata,
           CODEDstrata = pl_codedstrata,
           CCDCSMAstrata = pl_ccdcsmastrata,
           GEEcombo_strata = pl_agreementchangestrata,
           GEEcombo_strata_readable = pl_readable)
  
  #################################
  # Prep for Analysis
  #################################
  
  # get ready to calculate agreement for each map with CEO data
  # make new columns with readable strata names for each model
  
  # DEG:1
  # LOSS:2
  # GAIN:3
  # Nonforest:4
  # Forest:5
  
  #Landtrendr
  FINALDATASET$LT_strata_readable <- ifelse(FINALDATASET$LTstrata == 5, 'stable forest',
                                            ifelse(FINALDATASET$LTstrata == 1, 'forest degradation',
                                                   ifelse(FINALDATASET$LTstrata == 2, 'forest loss',
                                                          ifelse(FINALDATASET$LTstrata == 4, 'stable non forest',
                                                                 ifelse(FINALDATASET$LTstrata == 3, 'forest gain',
                                                                        'NotReviewed')))))
  # table(FINALDATASET$LT_strata_readable)
  
  #Machine Learning
  FINALDATASET$ML_strata_readable <- ifelse(FINALDATASET$MTDDstrata == 5, 'stable forest',
                                            ifelse(FINALDATASET$MTDDstrata == 1, 'forest degradation',
                                                   ifelse(FINALDATASET$MTDDstrata == 2, 'forest loss',
                                                          ifelse(FINALDATASET$MTDDstrata == 4, 'stable non forest',
                                                                 ifelse(FINALDATASET$MTDDstrata == 3, 'forest gain',
                                                                        'NotReviewed')))))
  
  # table(FINALDATASET$ML_strata_readable)
  
  #CODED
  FINALDATASET$CODED_strata_readable <- ifelse(FINALDATASET$CODEDstrata == 5, 'stable forest',
                                               ifelse(FINALDATASET$CODEDstrata == 1, 'forest degradation',
                                                      ifelse(FINALDATASET$CODEDstrata == 2, 'forest loss',
                                                             ifelse(FINALDATASET$CODEDstrata == 4, 'stable non forest',
                                                                    ifelse(FINALDATASET$CODEDstrata == 3, 'forest gain',
                                                                           'NotReviewed')))))
  
  # table(FINALDATASET$CODED_strata_readable)
  
  #CCDC-SMA
  FINALDATASET$CS_strata_readable <- ifelse(FINALDATASET$CCDCSMAstrata == 5, 'stable forest',
                                            ifelse(FINALDATASET$CCDCSMAstrata == 1, 'forest degradation',
                                                   ifelse(FINALDATASET$CCDCSMAstrata == 2, 'forest loss',
                                                          ifelse(FINALDATASET$CCDCSMAstrata == 4, 'stable non forest',
                                                                 ifelse(FINALDATASET$CCDCSMAstrata == 3, 'forest gain',
                                                                        'NotReviewed')))))
  
  # table(FINALDATASET$CS_strata_readable)
  
  # combined GEE agreement strata
  FINALDATASET$GEEcombo_strata_readable2 <- ifelse(FINALDATASET$GEEcombo_strata == 5, 'stable forest',
                                                   ifelse(FINALDATASET$GEEcombo_strata == 1, 'forest degradation',
                                                          ifelse(FINALDATASET$GEEcombo_strata == 2, 'forest loss',
                                                                 ifelse(FINALDATASET$GEEcombo_strata == 4, 'stable non forest',
                                                                        ifelse(FINALDATASET$GEEcombo_strata == 3, 'forest gain',
                                                                               'NotReviewed')))))
  
  # table(FINALDATASET$GEEcombo_strata_readable2)
  
  # Make agreement tables ######################################################
  
  # make the agreement tables for each individual map
  # (named SMEagreement_XXX, XXX = model name)
  
  # Time 1 ---------------------------------------------------------------------
  FINALDATASET$t1_LTcompare
  
  #LandTrendr
  FINALDATASET$t1_LTcompare <- ifelse(FINALDATASET$t1_disturbance_type == FINALDATASET$LT_strata_readable &
                                        !is.na(FINALDATASET$t1_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t1_disturbance_type != FINALDATASET$LT_strata_readable &
                                               !is.na(FINALDATASET$t1_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  # table(FINALDATASET$LTcompare)
  t1_SMEagreement_LandTrendr <- table(FINALDATASET$t1_LTcompare)
  
  #Machine Learning
  FINALDATASET$t1_MLcompare <- ifelse(FINALDATASET$t1_disturbance_type == FINALDATASET$ML_strata_readable &
                                        !is.na(FINALDATASET$t1_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t1_disturbance_type != FINALDATASET$ML_strata_readable &
                                               !is.na(FINALDATASET$t1_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  
  # table(FINALDATASET$MLcompare)
  t1_SMEagreement_MachineLearning <- table(FINALDATASET$t1_MLcompare)
  
  #CODED
  FINALDATASET$t1_CODEDcompare <- ifelse(FINALDATASET$t1_disturbance_type == FINALDATASET$CODED_strata_readable &
                                           !is.na(FINALDATASET$t1_disturbance_type),
                                         'Agree',
                                         ifelse(FINALDATASET$t1_disturbance_type != FINALDATASET$CODED_strata_readable &
                                                  !is.na(FINALDATASET$t1_disturbance_type),
                                                'Dis',
                                                'NotReviewed'))
  
  # table(FINALDATASET$CODEDcompare)
  t1_SMEagreement_CODED <- table(FINALDATASET$t1_CODEDcompare)
  
  #CCDC-SMA
  FINALDATASET$t1_CScompare <- ifelse(FINALDATASET$t1_disturbance_type == FINALDATASET$CS_strata_readable &
                                        !is.na(FINALDATASET$t1_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t1_disturbance_type != FINALDATASET$CSE_strata_readable &
                                               !is.na(FINALDATASET$t1_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  
  # table(FINALDATASET$CScompare)
  t1_SMEagreement_CCDCSMA <- table(FINALDATASET$t1_CScompare)
  
  
  #GEEcombo
  FINALDATASET$t1_GEEcombocompare <- ifelse(FINALDATASET$t1_disturbance_type == FINALDATASET$GEEcombo_strata_readable2 &
                                              !is.na(FINALDATASET$t1_disturbance_type),
                                            'Agree',
                                            ifelse(FINALDATASET$t1_disturbance_type != FINALDATASET$GEEcombo_strata_readable2 &
                                                     !is.na(FINALDATASET$t1_disturbance_type),
                                                   'Dis',
                                                   'NotReviewed'))
  
  # table(FINALDATASET$GEEcombocompare)
  t1_agreement_GEEcombo <- table(FINALDATASET$t1_GEEcombocompare)
  
  # Time 2 ---------------------------------------------------------------------
  
  # make the agreement tables for each individual map (named SMEagreement_XXX, XXX = map name) ################
  #LandTrendr
  FINALDATASET$t2_LTcompare <- ifelse(FINALDATASET$t2_disturbance_type == FINALDATASET$LT_strata_readable &
                                        !is.na(FINALDATASET$t2_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t2_disturbance_type != FINALDATASET$LT_strata_readable &
                                               !is.na(FINALDATASET$t2_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  # table(FINALDATASET$LTcompare)
  t2_SMEagreement_LandTrendr <- table(FINALDATASET$t2_LTcompare)
  
  #Machine Learning
  FINALDATASET$t2_MLcompare <- ifelse(FINALDATASET$t2_disturbance_type == FINALDATASET$ML_strata_readable &
                                        !is.na(FINALDATASET$t2_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t2_disturbance_type != FINALDATASET$ML_strata_readable &
                                               !is.na(FINALDATASET$t2_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  
  # table(FINALDATASET$MLcompare)
  t2_SMEagreement_MachineLearning <- table(FINALDATASET$t2_MLcompare)
  
  #CODED
  FINALDATASET$t2_CODEDcompare <- ifelse(FINALDATASET$t2_disturbance_type == FINALDATASET$CODED_strata_readable &
                                           !is.na(FINALDATASET$t2_disturbance_type),
                                         'Agree',
                                         ifelse(FINALDATASET$t2_disturbance_type != FINALDATASET$CODED_strata_readable &
                                                  !is.na(FINALDATASET$t2_disturbance_type),
                                                'Dis',
                                                'NotReviewed'))
  
  # table(FINALDATASET$CODEDcompare)
  t2_SMEagreement_CODED <- table(FINALDATASET$t2_CODEDcompare)
  
  #CCDC-SMA
  FINALDATASET$t2_CScompare <- ifelse(FINALDATASET$t2_disturbance_type == FINALDATASET$CS_strata_readable &
                                        !is.na(FINALDATASET$t2_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t2_disturbance_type != FINALDATASET$CS_strata_readable &
                                               !is.na(FINALDATASET$t2_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  
  # table(FINALDATASET$CScompare)
  t2_SMEagreement_CCDCSMA <- table(FINALDATASET$t2_CScompare)
  
  
  #GEEcombo
  FINALDATASET$t2_GEEcombocompare <- ifelse(FINALDATASET$t2_disturbance_type == FINALDATASET$GEEcombo_strata_readable2 &
                                              !is.na(FINALDATASET$t2_disturbance_type),
                                            'Agree',
                                            ifelse(FINALDATASET$t2_disturbance_type != FINALDATASET$GEEcombo_strata_readable2 &
                                                     !is.na(FINALDATASET$t2_disturbance_type),
                                                   'Dis',
                                                   'NotReviewed'))
  
  # table(FINALDATASET$GEEcombocompare)
  t2_agreement_GEEcombo <- table(FINALDATASET$t2_GEEcombocompare)
  
  # Time 3 ---------------------------------------------------------------------
  
  # make the agreement tables for each individual map (named SMEagreement_XXX, XXX = map name) ################
  #LandTrendr
  FINALDATASET$t3_LTcompare <- ifelse(FINALDATASET$t3_disturbance_type == FINALDATASET$LT_strata_readable &
                                        !is.na(FINALDATASET$t3_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t3_disturbance_type != FINALDATASET$LT_strata_readable &
                                               !is.na(FINALDATASET$t3_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  # table(FINALDATASET$LTcompare)
  t3_SMEagreement_LandTrendr <- table(FINALDATASET$t3_LTcompare)
  
  #Machine Learning
  FINALDATASET$t3_MLcompare <- ifelse(FINALDATASET$t3_disturbance_type == FINALDATASET$ML_strata_readable &
                                        !is.na(FINALDATASET$t3_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t3_disturbance_type != FINALDATASET$ML_strata_readable &
                                               !is.na(FINALDATASET$t3_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  
  # table(FINALDATASET$MLcompare)
  t3_SMEagreement_MachineLearning <- table(FINALDATASET$t3_MLcompare)
  
  #CODED
  FINALDATASET$t3_CODEDcompare <- ifelse(FINALDATASET$t3_disturbance_type == FINALDATASET$CODED_strata_readable &
                                           !is.na(FINALDATASET$t3_disturbance_type),
                                         'Agree',
                                         ifelse(FINALDATASET$t3_disturbance_type != FINALDATASET$CODED_strata_readable &
                                                  !is.na(FINALDATASET$t3_disturbance_type),
                                                'Dis',
                                                'NotReviewed'))
  
  # table(FINALDATASET$CODEDcompare)
  t3_SMEagreement_CODED <- table(FINALDATASET$t3_CODEDcompare)
  
  #CCDC-SMA
  FINALDATASET$t3_CScompare <- ifelse(FINALDATASET$t3_disturbance_type == FINALDATASET$CS_strata_readable &
                                        !is.na(FINALDATASET$t3_disturbance_type),
                                      'Agree',
                                      ifelse(FINALDATASET$t3_disturbance_type != FINALDATASET$CS_strata_readable &
                                               !is.na(FINALDATASET$t3_disturbance_type),
                                             'Dis',
                                             'NotReviewed'))
  
  # table(FINALDATASET$CScompare)
  t3_SMEagreement_CCDCSMA <- table(FINALDATASET$t3_CScompare)
  
  
  #GEEcombo combo
  FINALDATASET$t3_GEEcombocompare <- ifelse(FINALDATASET$t3_disturbance_type == FINALDATASET$GEEcombo_strata_readable2 &
                                              !is.na(FINALDATASET$t3_disturbance_type),
                                            'Agree',
                                            ifelse(FINALDATASET$t3_disturbance_type != FINALDATASET$GEEcombo_strata_readable2 &
                                                     !is.na(FINALDATASET$t3_disturbance_type),
                                                   'Dis',
                                                   'NotReviewed'))
  
  # table(FINALDATASET$GEEcombocompare)
  t3_agreement_GEEcombo <- table(FINALDATASET$t3_GEEcombocompare)
  
  # FINALDATASET$strata = FINALDATASET$GEEcombo_strata_readable2
  
  # ----------------------------------------------------------------------------
  
  #################################
  # Analysis
  #################################
  
  # Agreement (confusion) matrix ###############################################
  
  # Time 1 ---------------------------------------------------------------------
  
  #cross tabulation of strata by disturbance type
  t1_table_GEEstrata_CEOdisttype <- table(FINALDATASET$GEEcombo_strata_readable2, FINALDATASET$t1_disturbance_type)
  write.csv(t1_table_GEEstrata_CEOdisttype, file = paste0(results_filepath, '.t1_confusionmatrix_GEEstrata_CEOdisttype-ART-TREES-', part, '.csv'), row.names = T)
  
  # Time 2 ---------------------------------------------------------------------
  
  #cross tabulation of strata by disturbance type
  t2_table_GEEstrata_CEOdisttype <- table(FINALDATASET$GEEcombo_strata_readable2, FINALDATASET$t2_disturbance_type)
  write.csv(t2_table_GEEstrata_CEOdisttype, file = paste0(results_filepath, 't2_confusionmatrix_GEEstrata_CEOdisttype-ART-TREES-', part, '.csv'), row.names = T)
  
  # Time 3 ---------------------------------------------------------------------
  
  #cross tabulation of strata by disturbance type
  t3_table_GEEstrata_CEOdisttype <- table(FINALDATASET$GEEcombo_strata_readable2, FINALDATASET$t3_disturbance_type)
  write.csv(t3_table_GEEstrata_CEOdisttype, file = paste0(results_filepath, 't3_confusionmatrix_GEEstrata_CEOdisttype-ART-TREES-', part, '.csv'), row.names = T)
  
  # ----------------------------------------------------------------------------
  
  # export final dataset #######################################################
  
  # final data set
  write.csv(FINALDATASET,
            file = paste0(results_filepath, 'CompiledData_CEO_GEE-ART-TREES-', part, '.csv'),
            row.names = F)
  
  # export data problems #######################################################
  
  # combine data issues into a single string
  dataproblems_string = paste("\nCEO QAQC data - plot differences between assessors:",
                              as.character(paste(unique(dataCEO$plotid_unique[!is.na(dataCEO$plot_differences_between_assessors__CEOqual)]), collapse = ", ")),
                              "\nCEO QAQC - sample differences within plot:",
                              as.character(paste(unique(dataCEO$plotid_unique[!is.na(dataCEO$sample_differences_within_plot__CEOqual)]), collapse = ", ")),
                              "\nIn time period 1, these plots did not have changes that agreed with the previous answer:",
                              paste(notcorrectchanges_t1, collapse=', '),
                              "\nChange type:",
                              as.character(paste(dataCEO$t1_change[dataCEO$t1_changeQA == FALSE], collapse='\n')),
                              "\nLand cover type in t0:",
                              as.character(paste(dataCEO$t0_type[dataCEO$t1_changeQA == FALSE], collapse='\n')),
                              "\nLand cover type in t1:",
                              as.character(paste(dataCEO$t1_type[dataCEO$t1_changeQA == FALSE], collapse='\n')),
                              "\nIn time period 2, these plots did not have changes that agreed with the previous answer:",
                              paste(notcorrectchanges_t2, collapse=', '),
                              "\nChange type:",
                              as.character(paste(dataCEO$t2_change[dataCEO$t2_changeQA == FALSE], collapse='\n')),
                              "\nLand cover type in t1:",
                              as.character(paste(dataCEO$t1_type[dataCEO$t2_changeQA == FALSE], collapse='\n')),
                              "\nLand cover type in t2:",
                              as.character(paste(dataCEO$t2_type[dataCEO$t2_changeQA == FALSE], collapse='\n')),
                              sep = "\n\n") 
  
  # print(dataproblems_string)
  
  # print list of data issues to a word document
  writeLines(dataproblems_string, paste0(results_filepath, 'CEO_data_problems_ART-TREES-', part, '.doc'))
  
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
  
  
  
  outlst <- dataproblems_string
  # agreement_simple, agreement, # 1, 2
  #                SMEagreement_LandTrendr, LT_Compare_Table, # 3.1
  #                SMEagreement_MachineLearning, ML_Compare_Table, # 3.2
  #                SMEagreement_CODED, CODED_Compare_Table,
  #                SMEagreement_CCDCSMA, CCDCSMA_Compare_Table,
  #                agreement_GEEcombo, GEEcombo_Compare_Table,  # 3.5
  #                FINALDATASET)  # 4
  return(outlst)
  # outlst
  
}

analyze(dataCEO, 
          dataGEE,
          dataCEO_qual, 
          dataGEE_qual,
          strataAreasGEE)

