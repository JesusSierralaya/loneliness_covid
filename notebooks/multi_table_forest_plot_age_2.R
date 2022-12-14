rm(data_table_1)

# IMPROVE THE CURRENTLY TABLE + FORESTPLOT

library(tidyverse)
library(gridExtra)

# DATA ------------------------------------------------

## Variables
source("throughput/import_clean.R", encoding = 'UTF-8')

## Transformation
source("throughput/multivariate.R", encoding = 'UTF-8')

## Table with the all values

tbl_multi_values <- tbl_multi$table_body

# TABLE -----------------------------------------------

## First row (names col) ------------------------------

# # Letter A for first cattegories
# row_letter <- LETTERS[1]

# Add names_cols content
names_cols <- c("VARIABLE", 
                "LEVEL",
                "BETA")

# Number of columns
n_cols <- length(names_cols)

# Data base

data_table_1 <- tibble(
  rows_table = LETTERS[1],
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

# # Letter B for second variable
# row_letter <- LETTERS[2]

# Add variable name and empty spaces for names_cols content
variable_label_extract <- 
  data_age |> 
  select(label) |> 
  filter(row_number()==1) |> pull()

variable_label <- c(variable_label_extract, rep("",(n_cols-1)))

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

for (i in 1:n_cat) {
  data_table_1 <- data_table_1 |> add_row(
    rows_table = LETTERS[i+1],
    cols_table = 1:n_cols,
    names_cols = c(variable_label[i], level_label[i], beta_label[i])
  )
  rm(i)
}

## Sex -----------------------------------

# # data_sex <- 
#   tbl_multi_values |>
#   # filter(variable == "age") |> # editable
# 
# # Number of categories to letters - reference and label
# n_rows <- nrow(data_age)
# n_cat <- n_rows - 2

## FINAL ORDER --------------------------------------------

data_table_1 <- 
  data_table_1 |> mutate(
    rows_table = rows_table |> as_factor() |> 
      fct_relevel(sort(LETTERS[1:(n_rows-1)], decreasing = TRUE))
  ) 

## PLOT -------------------------

table_plot <- data_table_1 |> 
  ggplot(aes(x = cols_table, y = rows_table,
             label = names_cols)) +
  geom_text(size = 5, hjust=0, vjust=0.5) +
  theme_bw() +
  labs(x="",y="") +
  coord_cartesian(xlim= c(1, (n_cols+.5)))


# FORESTPLOT ----------------------------------------------

data_foresplot <- 
  data_age |> select(-label) |> 
  slice((n_cat-1):n_rows) |> 
  transmute(
    group = LETTERS[1:(n_cat+1)] |> as_factor() |> 
      fct_relevel(sort(LETTERS[1:n_cat+1], decreasing = TRUE)),
    cen = estimate,
    low = conf.low,
    high = conf.high
  ) 

foresplot_data <- data_foresplot |> 
  ggplot(aes(cen,group)) + 
  geom_point(size=5, shape=18) +
  geom_errorbarh(aes(xmax = high, xmin = low), height = 0.15) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks = seq(0,14,1), labels = seq(0,14,1)) +
  labs(x="Beta", y="") +
  theme_bw() 

# GRID ARRANTE -----------------------------------

grid.arrange(table_plot, foresplot_data, ncol=2)







































