# ··············································································
# FILE NAME:   multivariate.R
# DESCRIPTION: Analysis multivariate
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        23/08/2022
# 
# ·············································································· 

## ---- INCLUDES: --------------------------------------------------------------

library(here)
library(gtsummary)
library(sjPlot)
library(survey)
library(magrittr)

# source("throughput/import_clean.R", encoding = 'UTF-8')

## ---- UNIVARIATE: ------------------------------------------------------------

# Reference level by group

DB_pre_post %<>% 
  mutate(
    age = age %>% 
      fct_relevel("50-64"),
    maritalstatus = maritalstatus %>% 
      fct_relevel("Married or in partnership Divorced"),
    educlevel = educlevel %>% 
      fct_relevel("Tertiary"),
    socialchanges_post = socialchanges_post %>% 
      fct_relevel("No"),
    economyworsened_post = economyworsened_post %>% 
      fct_relevel("No"),
    unemployment_post = unemployment_post %>% 
      fct_relevel("No"),
    materialdeprivation_pre = materialdeprivation_pre %>% 
      fct_relevel("No"),
    livingalone_post = livingalone_post %>% 
      fct_relevel("No")
  )

# Univariate with Survey design
tbl_univ <-
  svydesign(
  data = DB_pre_post,
  ids = ~ID_ECS,
  weights = DB_pre_post %>% pull(weights)
) %>% 
  tbl_uvregression(
    y = loneliness_post,
    method = survey::svyglm,
    formula = "{y} ~ {x} + offset(loneliness_pre)",
    include = -c(ID_ECS, loneliness_pre, weights)
    ) %>% 
  add_global_p(keep = TRUE)  %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE, hide_se = TRUE) %>% 
  bold_p() %>% 
  italicize_levels() %>% 
  bold_labels() %>% 
  modify_column_hide(stat_n)

## ---- MULTIVARIATE: ------------------------------------------------------------

fit_multi <-
  svydesign(
  data = DB_pre_post,
  ids = ~ID_ECS,
  weights = DB_pre_post %>% pull(weights)
  ) %>% 
  svyglm(
    formula = loneliness_post ~ offset(loneliness_pre) 
    # Fix
    + age
    + sex
    + educlevel
    + maritalstatus
    # random
    + virtualcontact_post
    + socialchanges_post
    + economyworsened_post
    + unemployment_post
    + depression_pre
    + depression_post
    + neuroticism_pre
    + extraversion_pre
    + disability_pre
    ) 

tbl_multi <- 
  fit_multi %>% 
  tbl_regression(
    label = list(age ~ labels_pre_post[labels_pre_post == "age",2],
                 sex ~ labels_pre_post[labels_pre_post == "sex",2],
                 educlevel ~ labels_pre_post[labels_pre_post == "educlevel",2],
                 maritalstatus ~ labels_pre_post[labels_pre_post == "maritalstatus",2],
                 virtualcontact_post ~ labels_pre_post[labels_pre_post == "virtualcontact_post",2],
                 socialchanges_post ~ labels_pre_post[labels_pre_post == "socialchanges_post",2],
                 economyworsened_post ~ labels_pre_post[labels_pre_post == "economyworsened_post",2],
                 unemployment_post ~ labels_pre_post[labels_pre_post == "unemployment_post",2],
                 depression_pre ~ labels_pre_post[labels_pre_post == "depression_pre",2],
                 depression_post ~ labels_pre_post[labels_pre_post == "depression_post",2],
                 neuroticism_pre ~ labels_pre_post[labels_pre_post == "neuroticism_pre",2],
                 extraversion_pre ~ labels_pre_post[labels_pre_post == "extraversion_pre",2],
                 disability_pre ~ labels_pre_post[labels_pre_post == "disability_pre",2]
                 ),
    # pvalue_fun = purrr::partial(style_sigfig, digits = 3),
    pvalue_fun = function(x) style_pvalue(x, digits = 3),
    show_single_row = c(economyworsened_post, unemployment_post,
                        depression_pre, depression_post)
  ) |>
  # add_global_p(keep = TRUE)  |> 
  add_q(
    method = "bonferroni",
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) |> 
  bold_p(t = 0.05, q = TRUE) |> 
  add_vif()

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
  # scale_x_discrete(limits = factor(1:n_cols)) +
  labs(x="", y="") +
  xlim(c(1,1.5)) 

# FOREST PLOT
data_foresplot <- 
  tbl_multi_values |> 
  select(estimate, q.value, starts_with("conf")) |> 
  add_row(
    estimate = NA, 
    conf.low = NA,
    conf.high = NA,
    .before = 1
  )  |> 
  mutate(
    sig = case_when(
      q.value > 0.05 ~ "",
      q.value > 0.01 ~ "*",
      q.value > 0.001 ~ "**",
      !is.na(q.value) ~ "***",
      TRUE ~ NA_character_
    ),
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
  geom_text(aes(label = sig), nudge_y = .35) +
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
    q.value == 1 ~ "> 0.999",
    q.value < 0.001 ~ "< 0.001",
    q.value < 0.01 ~ "< 0.01",
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

forest_table_plot <- (plot_col_labels + x_white + forest_plot + table_plot + x_white + plot_layout(widths = c(1.6,1.3, 1.9))) * 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()) + plot_annotation(
          title = "Multivariate plot",
          subtitle = "editable subtitle",
          caption = "*Applied Bonferroni correction"
        ) & theme(text = element_text('serif', size = 14))
