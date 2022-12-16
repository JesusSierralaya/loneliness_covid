library(tidyverse)
library(gridExtra) # grid.arrange
library(patchwork)

# DATA ------------------------------------------------

## Variables
source("throughput/import_clean.R", encoding = 'UTF-8')

## Transformation
source("throughput/multivariate.R", encoding = 'UTF-8')

rm(list=setdiff(ls(), "tbl_multi"))

# DATA
tbl_multi_values <- 
  tbl_multi$table_body |> 
  select(variable, label, estimate, starts_with("conf")) 

#EXAMPLES AND USEFUL CODE -----------------------------
# Example add coordinates for plot (easy way)
# data <- tbl_multi_values |> mutate(
#   x = 1, y = nrow(tbl_multi_values):1,
#   label_test = label
# ) 
# data |> ggplot(aes(x, y, label = label)) + 
#   geom_text(size = 5, hjust=0, vjust=0.5) +
#   coord_cartesian(xlim = c(.5, 2.5))

# Example 2 add coordinates for plot (easy way)
# data <- 
#   tbl_multi_values |> mutate(
#   x = 1, 
#   y = nrow(tbl_multi_values):1,
#   label = label
# ) 
# data |> ggplot(aes(x, y, label = label)) + 
#   geom_text(size = 5, hjust=0, vjust=0.5) +
#   coord_cartesian(xlim = c(.5, 2.5))

# OBJECTIVE ------------------------------------------

#- First version easy to add columns

# NEEDED --------------------------------------------

#- 1 vector content of the text
#- 1 vector x coordinates 
#- 1 vector y coordinates

# THEN --------------------------------------------

#- Add ref

# BUILD VECTOR CONTENT -------------------------------

# First col: label (variables name, values names, refs.)
col_1 <- 
  tbl_multi_values |> 
  select(label) |> pull() |> as.character()

# TRY AUTOMATITATION
# Format 
# duplicated(c(1, 3, 3, 4))
# which(c(TRUE, FALSE, TRUE))

# Manual
indexation <- "    "
# Age
col_1[2] <- paste0(indexation, "ref(",col_1[2],")")
col_1[3] <- paste(indexation, col_1[3])
col_1[4] <- paste(indexation, col_1[4])
col_1[5] <- paste(indexation, col_1[5])
# Sex
col_1[7] <- paste0(indexation, "ref(",col_1[7],")")
col_1[8] <- paste(indexation, col_1[8])
# Ed
col_1[10] <- paste0(indexation, "ref(",col_1[10],")")
col_1[11] <- paste(indexation, col_1[11])
col_1[12] <- paste(indexation, col_1[12])
col_1[13] <- paste(indexation, col_1[13])
# Marital
col_1[15] <- paste0(indexation, "ref(",col_1[15],")")
col_1[16] <- paste(indexation, col_1[16])
col_1[17] <- paste(indexation, col_1[17])
# Virtual
col_1[19] <- paste0(indexation, "ref(",col_1[19],")")
col_1[20] <- paste(indexation, col_1[20])
col_1[21] <- paste(indexation, col_1[21])
col_1[22] <- paste(indexation, col_1[22])
# Social changes
col_1[24] <- paste0(indexation, "ref(",col_1[24],")")
col_1[25] <- paste(indexation, col_1[25])
col_1[26] <- paste(indexation, col_1[26])

# Second col: beta
col_2 <- tbl_multi_values |> 
  select(estimate) |> round(2) |>  
  mutate_all(as.character)|> 
  replace_na(list(estimate = "")) |> pull()

# Third col:

# 
n_cols <- 2

# NAME COLS (FIRST ROW NAMES) ----------------------

names_col <- c("VARIABLES", "BETA")

# Combine one vector
# add names cols
content_col <- c(names_col[1], col_1, 
                 names_col[2], col_2)

# BUILD VECTOR X COORDINATE --------
# Dependencies
n_rows <- length(content_col)/n_cols

# Variables
x_coord <- c(rep(1, n_rows),
             rep(n_cols, n_rows))

# BUILD VECTOR Y COORDINATE --------

# Variables
y_coord <- rep(n_rows:1, n_cols)


# TEST PLOT --------

data_table <- tibble(
  content_col,
  x_coord,
  y_coord
) 

table_plot <- data_table |> 
  ggplot(aes(x_coord, y_coord, 
                label = content_col)) +
  geom_text(size = 5, hjust=0, vjust=0.5) +
  theme_bw() +
  scale_y_discrete(limits = factor(n_rows:1)) +
  scale_x_discrete(limits = factor(1:n_cols)) +
  labs(x="", y="") +
  xlim(c(1,3.5))  

# FOREST PLOT 

data_foresplot <- tbl_multi_values |> 
  select(estimate, starts_with("conf")) |> 
  add_row(
  estimate = NA, 
  conf.low = NA,
  conf.high = NA,
  .before = 1
) |> mutate(
  group = n_rows:1
)

forest_plot <- data_foresplot |> 
  ggplot(aes(estimate,group)) + 
  geom_point(size=5, shape=18) +
  geom_errorbarh(aes(xmax = conf.high, 
                     xmin = conf.low), 
                 height = 0.15) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  theme_bw() +
  scale_y_discrete(limits = factor(n_rows:1)) +
  labs(x="Beta", y="") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# 
# grid.arrange(table_plot, forest_plot, ncol=2,
#              widths = c(.7, .3))

# hide grid + theme_void() 
table_plot + forest_plot +
  plot_layout(widths = c(2,1))  & theme_minimal() 


