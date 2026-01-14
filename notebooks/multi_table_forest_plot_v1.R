library(tidyverse)
library(gridExtra)

# DATA ------------------------------------------------

## Variables
source("throughput/import_clean.R", encoding = 'UTF-8')

## Transformation
source("throughput/multivariate.R", encoding = 'UTF-8')

rm(list=setdiff(ls(), "tbl_multi"))

tbl_multi_values <- 
  tbl_multi$table_body |> 
  select(variable, label, estimate) 

# TABLE ----------------------------------------------

## CONTENT COL ----------------------------------------

unique_var <- tbl_multi_values |> 
  distinct(variable) |> pull() 

n_var <- length(unique_var)

lab_content <- c("VARIABLES", "LEVEL", "BETA")

for (j in 1:n_var) {
  # select all rows
  db_one_var <- 
    tbl_multi_values |> 
    filter(variable %in% unique_var[j])# j
  
  # # select 1 label
  lab_1 <- db_one_var |> 
    select(label) |> 
    slice(1) |> # fix
    pull()
  
  # dim
  n_row <- nrow(db_one_var)
  n_col <- ncol(db_one_var)
  
  if (n_row == 5) {
    
    lab_test <- c(lab_1, rep("", (n_row-3)))
    
    # level and beta
    for (i in 1:n_col) {
      # select level
      lab_test[i+n_col] <- db_one_var |> 
        select(label) |> 
        slice(i+2) |> # fix
        pull()
      # select estimate
      lab_test[i+n_col+n_col] <- db_one_var |>
        select(estimate) |>
        slice(i+2) |> # fix
        pull() |> round(2)
      rm(i)
    }
    
    # order input way
    lab_test <- lab_test |> 
      matrix(ncol = n_col) |> t() |> c()
    
    # nrow 3 
  } else if (n_row == 3) {
    # variable name
    lab_test[1] <- lab_1
    
    # level and beta
    lab_test[2] <- db_one_var |> 
      select(label) |> 
      slice(n_row) |> # fix
      pull()
    
    lab_test[3] <- db_one_var |> 
      select(estimate) |> 
      slice(n_row) |> # fix
      pull()|> round(2)
    ## nrow 4
  }  else if (n_row == 4) {
    lab_test <- c(lab_1, rep("", (n_row-3)))
    # 
    # # level and beta
    for (i in 1:2) {
      #   # select level
      lab_test[i+2] <- db_one_var |>
        select(label) |>
        slice(i+2) |> # fix
        pull()
      # select estimate
      lab_test[i+4] <- db_one_var |>
        select(estimate) |>
        slice(i+2) |> # fix
        pull() |> round(2)
      rm(i)
    }
    # 
    # order input way
    lab_test <- lab_test |>
      matrix(ncol = n_col) |> t() |> c()
    
  }else{
    # nrow 1
    lab_test <- lab_1
    lab_test[2] <- ""
    lab_test[3]  <- db_one_var |>
      select(estimate) |>
      # slice(i+2) |> # fix
      pull() |> round(2) 
  }
  
  # everything only one vector
  lab_content <- c(lab_content, lab_test)
  # rm(lab_test)
  lab_test <- 0
  
  rm(j)
}

## ROW COL ----------------------------------------

rows_content <- length(lab_content) / n_col

lab_row <- LETTERS[1] |> rep(n_col)

for (i in 2:rows_content) {
  lab_row_container <- LETTERS[i] |> rep(n_col)
  lab_row <- c(lab_row, lab_row_container)
}


## COL COL ----------------------------------------

lab_col <- rep(1:3, rows_content)

## DATA FRAME 

data_table_1 <- tibble(
  rows_table = lab_row,
  cols_table = lab_col,
  names_col = lab_content,
)

# Order

data_table_1 <- 
  data_table_1 |> mutate(
    rows_table = rows_table |> as_factor() |> 
      fct_relevel(sort(LETTERS[1:rows_content], decreasing = TRUE))
  ) 


## PLOT TABLE -------------------------

table_plot <- data_table_1 |> 
  ggplot(aes(x = cols_table, y = rows_table,
             label = names_col)) +
  geom_text(size = 5, hjust=0, vjust=0.5) +
  theme_bw() +
  labs(x="",y="") +
  coord_cartesian(xlim= c(1, (n_col+.2)))

# FORESPLOT ----------------------------------------------

tbl_multi_values <- 
  tbl_multi$table_body |> 
  select(estimate, starts_with("conf")) 


data_foresplot <- tbl_multi_values |> 
  filter(!is.na(estimate)) |> 
  add_row(estimate = NA,
          conf.low = NA,
          conf.high = NA, .before = 1) |> 
  transmute(
    group = LETTERS[1:rows_content]|> as_factor() |> 
      fct_relevel(sort(LETTERS[1:rows_content], decreasing = TRUE)),
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

grid.arrange(table_plot, foresplot_data, ncol=2)
