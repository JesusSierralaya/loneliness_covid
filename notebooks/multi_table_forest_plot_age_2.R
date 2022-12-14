# IMPROVE THE CURRENTLY TABLE + FORESTPLOT

library(tidyverse)
library(gridExtra)

# DATA ------------------------------------------------

## Variables
source("throughput/import_clean.R", encoding = 'UTF-8')

## Transformation
source("throughput/multivariate.R", encoding = 'UTF-8')

## Table with the values

tbl_multi_values <- tbl_multi$table_body

# TABLE -----------------------------------------------

## First row (names col) ------------------------------

# Letter A for first cattegories
row_letter <- LETTERS[1]

# Add names_cols content
names_cols <- c("Variable", 
                "Level",
                "Beta")

# Number of columns
n_cols <- length(names_cols)

# Data base

data_table_1 <- tibble(
  rows_table = row_letter,
  cols_table = 1:n_cols,
  names_cols
)

## Age grouped -----------------------------------

data_age <- tbl_multi_values |>
  filter(variable == "age") |> # editable
  select(label, estimate, conf.low, conf.high)

# Number of categories to letters - reference and label
n_rows <- nrow(data_age)
n_cat <- n_rows - 2

# Letter B for second variable
row_letter <- LETTERS[2]

# Add variable name and empty spaces for names_cols content
variable_label_extract <- 
  data_age |> 
  select(label) |> 
  filter(row_number()==1) |> pull()

variable_label <- c(variable_label, rep("",(n_cols-1)))

# Add level names for names_cols content
level_label <- data_age |> 
  select(label) |> 
  slice(n_cat:n_rows) |> pull() 

# Add beta for names_cols content
beta_label <- data_age |> 
  select(estimate) |>
  # filter(label %in% 3:4) |> pull()
  slice(n_cat:n_rows)  |> pull() |> round(2)

# # Add to Data base
# 
data_table_1 |> add_row(
  rows_table = LETTERS[2:4],
  cols_table = 1:3, #1:n_cols,
  names_cols = c(variable_label, level_label, beta_label)
)

## Find a way to improve the previous lines









