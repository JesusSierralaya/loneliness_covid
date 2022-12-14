library(tidyverse)
library(gridExtra)

# DATA ------------------------------------------------

## Variables
source("throughput/import_clean.R", encoding = 'UTF-8')

## Transformation
source("throughput/multivariate.R", encoding = 'UTF-8')

## Table with the all values

tbl_multi_values <- 
  tbl_multi$table_body |> 
    # If we add also add in names_cols
    # select(variable, label, estimate, conf.low, conf.high) |> # variables
    select(variable, label, estimate) |> # variables
    print(n = 33)

# list/number of variables
  
tbl_multi_values |> distinct(variable) |> nrow()

# TABLE ----------------------------------------------------

# First row (names cols)

names_cols <- c("VARIABLE", 
                "LEVEL",
                "BETA")

# Number of columns
n_cols <- length(names_cols)

# Data base

first_row_table <- tibble(
  rows_table = LETTERS[1],
  cols_table = 1:n_cols,
  names_cols
)
