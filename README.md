# NEPAL-TREES-2023
Calculations helpers for TREES crediting level

### How to use

:exclamation::exclamation::exclamation: These scripts are intended to work with confidential data downloaded separately and won't work without it.

1. Download and extract the repository: Code > Download ZIP.
1. Run the R project file (requires Rstudio).
1. Create a folder "data" in the project folder: `.../NEPAL-TREES-2023/data`.
1. Download the CEO csv files from CollectEarthOnline ART/TREE surveys and move then to the R project's `data` subfolder.
1. The scripts should now be able to locate the data and run the analyses.

### 02 Aug 2023

- `R/AD-CEO-disagreements.R` 
  - Finds plot IDs for samples where 3 interpreters found different values for at least one variable.
  - For disagreement plots, detects all column names were the disagreements occur.


