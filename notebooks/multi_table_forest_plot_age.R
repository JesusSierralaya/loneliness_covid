library(tidyverse)
library(gridExtra)

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

# Data base with all data

table_multi <- tbl_multi$table_body

# TABLE --------------------------

## First row: names col -------------------

names_cols <- c("Variable", "Level", "Beta")# editable

num_row <- length(names_cols)

row_letter <- LETTERS[1] |> rep(num_row)

# data base first row

data_table <- tibble(
  rows_table = row_letter,
  cols_table = 1:num_row,
  names_cols
);data_table

## Variable age ------------


# Data base only for ages

data_age <- table_multi |>
  filter(variable == "age") |> # editable
  select(label, estimate, conf.low, conf.high)

# n categories

n_cat <- nrow(data_age)

# Var label

label_var <- data_age |> 
  select(label) |> filter(row_number()==1) |> pull()

label_var <- c(label_var, "", "")

# val label

# label_val <- data_age |> 
#   select(label) |> filter(row_number()==n_cat-2:n_cat) |> pull()

label_val <- 
  data_age |> 
  select(label) |>
  # filter(label %in% 3:4) |> pull()
  slice((n_cat-2):n_cat)  |> pull() 


# estimate

# label_beta <- data_age |> 
#   select(estimate) |> filter(row_number()==n_cat) |> pull() |> round(2)

label_beta <- 
  data_age |> 
  select(estimate) |>
  # filter(label %in% 3:4) |> pull()
  slice((n_cat-2):n_cat)  |> pull() |> round(2)

# merge row by row B

data_table <- data_table |> add_row(
  rows_table = LETTERS[2],# B B B
  cols_table = 1:num_row, # 1 2 3
  names_cols = c(label_var[1], label_val[1], label_beta[1]) # "Age grouped"             "18-34" "0.133918477829876" 
)

# merge row by row C
data_table <- data_table |> add_row(
  rows_table = LETTERS[3],# B B B
  coltibbles_table = 1:num_row, # 1 2 3
  names_cols = c(label_var[2], label_val[2], label_beta[2]) # "Age grouped"             "18-34" "0.133918477829876" 
)

# merge row by row D
data_table <- data_table |> add_row(
  rows_table = LETTERS[4],# B B B
  cols_table = 1:num_row, # 1 2 3
  names_cols = c(label_var[3], label_val[3], label_beta[3]) # "Age grouped"             "18-34" "0.133918477829876" 
)

# arrange like the example
lab <- data_table |> arrange(cols_table)

# order 

lab <- lab |> mutate(
  rows_table = rows_table |>
    ordered(sort(LETTERS[1:4], decreasing = TRUE))
)

# FOREST PLOT

# dat <- data_age |> select(-label) |> slice((n_cat-2):n_cat) |> 
#   transmute(
#     group = LETTERS[1:(n_cat-2)] |>
#       ordered(sort(LETTERS[1:4], decreasing = TRUE)),
#     cen = c(NA, estimate),
#     low = c(NA, conf.low),
#     high = c(NA, conf.high)
# )

dat <- data_age |> select(-label) |> slice((n_cat-3):n_cat) |> 
  transmute(
    group = LETTERS[1:(n_cat-1)] |>
      ordered(sort(LETTERS[1:4], decreasing = TRUE)),
    cen = estimate,
    low = conf.low,
    high = conf.high
  )


plot_1 <- ggplot(dat,aes(cen,group)) + 
  geom_point(size=5, shape=18) +
  geom_errorbarh(aes(xmax = high, xmin = low), height = 0.15) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks = seq(0,14,1), labels = seq(0,14,1)) +
  labs(x="Beta", y="")

# merge
data_tab <- ggplot(lab, aes(x = cols_table, 
                  y = rows_table, 
                  label = format(names_cols, nsmall = 1))) +
  geom_text(size = 4, hjust=0, vjust=0.5) + 
  theme_bw() +
  # geom_hline(aes(yintercept=c(6.5,7.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),#element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),#element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))

grid.arrange(data_tab, plot_1, ncol=2)
