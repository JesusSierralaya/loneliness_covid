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
  tbl_multi$table_body

# Format
indexation <- "    "

# col labels
col_labels <- tbl_multi_values |> 
  select(reference_row, label, row_type) |> 
  mutate(
    label_new = case_when(
      reference_row == TRUE ~ paste0(indexation, "Ref(",label, ")"),
      reference_row == FALSE & row_type == "level" ~ paste0(indexation, label),
      TRUE ~ label
    ) 
  ) |> 
  select(label_new) |>
  add_row(label_new = "VARIABLES", .before = 1) |> pull() |> as.character()

n_rows <- length(col_labels)

# Col labels to graph
data_col_labels <- 
  tibble(
  col_labels,
  x = 1,
  y = n_rows:1
) 

plot_col_labels <- data_col_labels |> 
  ggplot(aes(x, y, label = col_labels)) +
  geom_text(size = 5, hjust=0, vjust=0.5,
            family="serif") +
  theme_bw() +
  scale_y_discrete(limits = factor(n_rows:1)) +
  scale_x_discrete(limits = factor(1:n_cols)) +
  labs(x="", y="") +
  xlim(c(1,1.5)) 

# FOREST PLOT
data_foresplot <- 
  tbl_multi_values |> 
  select(estimate, starts_with("conf")) |> 
  add_row(
    estimate = NA, 
    conf.low = NA,
    conf.high = NA,
    .before = 1
  ) |> mutate(
    group = n_rows:1
  )

forest_plot <-
  data_foresplot |> 
  ggplot(aes(estimate,group)) + 
  geom_point(size=5, shape=18) +
  geom_errorbarh(aes(xmax = conf.high, 
                     xmin = conf.low), 
                 height = 0.15) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  theme_bw() +
  scale_y_discrete(limits = factor(n_rows:1)) +
  labs(x="Beta", y="") 

# SECOND TABLE

# Second col: beta
beta_col <- tbl_multi_values |> 
  select(estimate) |> round(2) |>  
  mutate_all(as.character)|> 
  # replace_na(list(estimate = "")) |> 
  pull()

# Third col: ci (together)
ci_col <-
  tbl_multi_values |> 
  select(ci) |> #mutate_all(as.character)|> 
  # replace_na(list(ci = "")) |> 
  pull()

col_beta_ci <- tibble(
  beta_col, ci_col
) |> mutate(
  col_23 = if_else(is.na(beta_col),
                   "",
                   paste0(beta_col, " (", ci_col, ")"))
) |> select(col_23) |>
  add_row(col_23 = "BETA (CI)", .before = 1) |>
  pull()

# q value (bonf correction)
col_qvalue <- 
  tbl_multi_values |> 
  select(q.value) |> 
  mutate(q.value = case_when(
    q.value == 1 ~ "0.99",
    q.value < 0.001 ~ "< 0.001",
    q.value |> is.na() ~ "",
    TRUE ~ q.value |> round(2) |> as.character()
  )) |> 
  add_row(q.value = "p-value*", .before = 1) |>
  pull()

# col: aGVIF
col_aGVIF <- tbl_multi_values |> 
  select(aGVIF) |> round(2) |> 
  mutate_all(as.character)|>
  replace_na(list(aGVIF = "")) |> 
  add_row(aGVIF = "aGVIF", .before = 1) |>
  pull()

content_col <- c(
  col_beta_ci, col_qvalue, col_aGVIF
)

n_cols <- 3 # number of col for this table

x_coord <- c(rep(1, n_rows),
             rep(1.7, n_rows),
             rep(2, n_rows))

y_coord <- rep(n_rows:1, n_cols)

# table
data_table <-
  tibble(
  content_col,
  x_coord,
  y_coord
) 

table_plot <- 
  data_table |> 
    ggplot(aes(x_coord, y_coord, 
               label = content_col)) +
    geom_text(size = 5, hjust=0, vjust=0.5,
              family="serif") +
    theme_bw() +
    scale_y_discrete(limits = factor(n_rows:1)) +
    scale_x_discrete(limits = factor(1:n_cols)) +
    labs(x="", y="") +
    xlim(c(1,2.2))  

  
  # TOTAL MERGE

x_white <- theme(axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank())

(plot_col_labels + x_white + forest_plot + table_plot + x_white + plot_layout(widths = c(1.6,1.3, 1.9))) * 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()) + plot_annotation(
          title = "Multivariate plot",
          subtitle = "editable subtitle",
          caption = "*Applied Bonferroni correction"
        ) & theme(text = element_text('serif', size = 14))

