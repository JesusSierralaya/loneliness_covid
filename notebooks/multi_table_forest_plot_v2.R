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

data <- tbl_multi_values |> mutate(
  x = 1, y = nrow(tbl_multi_values):1,
  label_test = label
) 


data |> ggplot(aes(x, y, label = label)) + 
  geom_text(size = 5, hjust=0, vjust=0.5) +
  coord_cartesian(xlim = c(.5, 2.5))

## test 2 ADD next col

data <- tbl_multi_values |> mutate(
  x = 1, 
  y = nrow(tbl_multi_values):1,
  label = label
) 

# ggplot
# data |> ggplot(aes(x, y, label = label)) + 
#   geom_text(size = 5, hjust=0, vjust=0.5) +
#   coord_cartesian(xlim = c(.5, 2.5))


# tbl_multi_values |> 
#   select_if(is.numeric) |> round(2) |> mutate_all(as.character)|> 
#   replace(is.na, "") |> pull()

label_1 <- tbl_multi_values |> select(label) |> pull() |> as.character()

estimate <- tbl_multi_values |> 
  select(estimate) |> round(2) |>  
  mutate_all(as.character)|> 
  replace_na(list(estimate = ""))

# combine in a single character vector label_1 estimate

  
