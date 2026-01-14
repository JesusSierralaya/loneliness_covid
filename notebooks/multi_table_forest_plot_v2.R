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
  tbl_multi$table_body# |> 
  # select(variable, label, estimate, starts_with("conf")) 

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
# col_1 <- 
#   tbl_multi_values |> 
#   select(label) |> pull() |> as.character()

# TRY AUTOMATITATION
# Format 
# duplicated(c(1, 3, 3, 4))
# which(c(TRUE, FALSE, TRUE))

# Manual
indexation <- "    "
# # Age
# col_1[2] <- paste0(indexation, "ref(",col_1[2],")")
# col_1[3] <- paste(indexation, col_1[3])
# col_1[4] <- paste(indexation, col_1[4])
# col_1[5] <- paste(indexation, col_1[5])
# # Sex
# col_1[7] <- paste0(indexation, "ref(",col_1[7],")")
# col_1[8] <- paste(indexation, col_1[8])
# # Ed
# col_1[10] <- paste0(indexation, "ref(",col_1[10],")")
# col_1[11] <- paste(indexation, col_1[11])
# col_1[12] <- paste(indexation, col_1[12])
# col_1[13] <- paste(indexation, col_1[13])
# # Marital
# col_1[15] <- paste0(indexation, "ref(",col_1[15],")")
# col_1[16] <- paste(indexation, col_1[16])
# col_1[17] <- paste(indexation, col_1[17])
# # Virtual
# col_1[19] <- paste0(indexation, "ref(",col_1[19],")")
# col_1[20] <- paste(indexation, col_1[20])
# col_1[21] <- paste(indexation, col_1[21])
# col_1[22] <- paste(indexation, col_1[22])
# # Social changes
# col_1[24] <- paste0(indexation, "ref(",col_1[24],")")
# col_1[25] <- paste(indexation, col_1[25])
# col_1[26] <- paste(indexation, col_1[26])

# Change labels
col_1 <- tbl_multi_values |> 
  select(reference_row, label, row_type) |> 
  mutate(
    label_new = case_when(
      reference_row == TRUE ~ paste0(indexation, "Ref(",label, ")"),
      reference_row == FALSE & row_type == "level" ~ paste0(indexation, label),
      TRUE ~ label
    ) 
  ) |> 
  select(label_new) |> pull() |> as.character()


# Second col: beta
beta_col <- tbl_multi_values |> 
  select(estimate) |> round(2) |>  
  mutate_all(as.character)|> 
  # replace_na(list(estimate = "")) |> 
  pull()

# NEXT COLS
# ci
# 5 col q-value
# col aGVIF

# Third col: ci (together)
ci_col <-
  tbl_multi_values |> 
  select(ci) |> #mutate_all(as.character)|> 
  # replace_na(list(ci = "")) |> 
  pull()

#---MERGE COL 1 AND 2 ---------------------------- 
col_23 <- tibble(
  beta_col, ci_col
) |> mutate(
  col_23 = if_else(is.na(beta_col),
                   "",
                   paste0(beta_col, " (", ci_col, ")"))
) |> select(col_23) |> pull()

#----------------------------------------------- 

# 4 col: q value
col_4 <- 
 tbl_multi_values |> 
  select(q.value) |> 
  mutate(q.value = case_when(
    q.value == 1 ~ "0.99",
    q.value < 0.001 ~ "< 0.001",
    q.value |> is.na() ~ "",
    TRUE ~ q.value |> round(2) |> as.character()
    )) |> pull()

# 5 col: aGVIF
col_5 <- tbl_multi_values |> 
  select(aGVIF) |> round(2) |> 
  mutate_all(as.character)|>
  replace_na(list(aGVIF = "")) |> pull()


# NAME COLS (FIRST ROW NAMES) ----------------------
names_col <- c("VARIABLES", "BETA (CI)", "p-value*",
               "aGVIF")

# Number of cols 
n_cols <- ls(pattern = "col_") |> length()
# Combine one vector
# add names cols
content_col <- c(names_col[1], col_1, 
                 names_col[2], col_23,
                 names_col[3], col_4,
                 names_col[4], col_5
                 )

# BUILD VECTOR X COORDINATE --------
# Dependencies
n_rows <- length(content_col)/n_cols

# Variables
x_coord <- c(rep(1, n_rows),
             rep(2, n_rows),
             rep(2.5, n_rows),
             rep(2.8, n_rows))

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
  geom_text(size = 5, hjust=0, vjust=0.5,
            family="serif") +
  theme_bw() +
  scale_y_discrete(limits = factor(n_rows:1)) +
  scale_x_discrete(limits = factor(1:n_cols)) +
  labs(x="", y="") +
  xlim(c(1,3))  

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
  labs(x="Beta", y="") 
# 
# grid.arrange(table_plot, forest_plot, ncol=2,
#              widths = c(.7, .3))

# hide grid + theme_void() 
forestplot_table_plot <-(table_plot + 
    theme_void() +
    # theme(
    #   # axis.text.x = element_blank(),
    #   # axis.ticks.x = element_blank(),
    #   ) +
    forest_plot) & 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
  ) &
  plot_layout(widths = c(2.5,1))  
