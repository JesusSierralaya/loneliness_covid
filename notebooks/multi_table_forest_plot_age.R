# First test with own data

## call data

# The data that I need for this first version is 

# - Variable name
# - level 
# - beta
# - CI min
# - CI max

# Option 1: extract from table made with function gtsummary::tbl_regression (maybe it will be only way because was measured with weights)

source("throughput/import_clean.R", encoding = 'UTF-8')

source("throughput/multivariate.R", encoding = 'UTF-8')

table_multi <- tbl_multi$table_body 

# Variable age ------------

## First row: names col

# table_multi |> 
#   filter(variable == "age") |> 
#   select(var_label, label, estimate)

names_cols <- c("Variable", "Level", "Beta")

num_row <- length(names_cols)

row_letter <- LETTERS[1] |> rep(num_row)

data_table <- tibble(
  rows_table = row_letter,
  cols_table = 1:num_row,
  names_cols
);data_table

## Var age

data_age <- table_multi |>
  filter(variable == "age") |>
  select(var_label, label, estimate)

# Var label

label_var <- data_age |> select(label) |> filter(row_number()==1) |> pull()

label_val <- data_age |> select(label) |> filter(row_number()==3) |> pull()

label_beta <- data_age |> select(estimate) |> filter(row_number()==3) |> pull() |> round(2)


data_table |> add_row(
  rows_table = LETTERS[2],
  cols_table = 1:num_row,
  names_cols = c(label_var, label_val, label_beta)
  )

# level label
