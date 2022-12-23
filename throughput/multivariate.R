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

# library(here)
library(gtsummary)
# library(sjPlot)
library(survey)
library(magrittr)
# table forestplot
library(tidyverse)
# library(gridExtra) # grid.arrange
# library(patchwork)

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
      fct_relevel("Unchanged"),
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

# Data extracted from gtsummary::tbl_regression
tbl_multi_values <- 
  tbl_multi$table_body

# Indexation reference level
indexation <- "    "

# Data base to plot
new_data <- 
  tbl_multi_values |> 
  # select(reference_row, header_row, label) |>
  mutate(
    # First col labels
    new_label = case_when(
      reference_row == TRUE ~ 
        paste0(label |> lag(), " (ref.: ",label, ")"), # var + ref
      reference_row == FALSE & header_row == FALSE ~ 
        paste0(indexation, label), # rest of levels
      is.na(header_row) ~ label # rest of variables
    ),
    # Forestplot sig mark
    sig = case_when(
      q.value > 0.05 ~ "",
      q.value > 0.01 ~ "*",
      q.value > 0.001 ~ "**",
      !is.na(q.value) ~ "***",
      TRUE ~ NA_character_
    ),
    # beta 
    beta = estimate |> round(2) |> as.character(),
    # CI
    new_ci = if_else(is.na(ci), "", paste0("(", ci, ")")), 
    # P-value with Bonferroni correction
    new_q.value = case_when(
      q.value == 1 ~ "> 0.999",
      q.value < 0.001 ~ "< 0.001",
      q.value < 0.01 ~ "< 0.01",
      q.value |> is.na() ~ "",
      TRUE ~ q.value |> round(2) |> as.character()
    )
  ) |> 
  # delete rows without info
  filter(!is.na(new_label)) |> 
  # Add title row
  add_row(new_label = "Variable",
          beta = "Beta",
          new_ci = "(CI 95%)",
          new_q.value = "p-value^a",
          .before = 1) 

# Draw plot

# Position cols
start_first_col <- -6
start_second_col <- 2
start_third_col <- 2.6
start_fourth_col <- 4.2

# format letter
size_letter <- 5
type_letter <- "serif"

forest_table_plot <- new_data |> 
  ggplot(aes(y = (new_label |> length()):1)) +
  # xlim(c(start_first_col, 5)) +
  geom_text(aes(x = start_first_col, 
                label = new_label), 
            size = size_letter, hjust=0, vjust=0.5, 
            family = type_letter) +
  # forestplot
  geom_point(aes(estimate),size = 5, shape = 18, 
             na.rm = TRUE)  +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), 
                 height = .15, na.rm = TRUE) + 
  geom_vline(xintercept = 0, linetype = "longdash") +
  # Sign mark
  geom_text(aes(estimate, label = sig), nudge_y = .35, 
            na.rm = TRUE) +
  # Beta
  geom_text(aes(x = start_second_col, label = beta),
            size = size_letter, hjust=0, vjust=0.5, 
            family = type_letter, na.rm = TRUE) +
  # ci
  geom_text(aes(x = start_third_col, label = new_ci),
            size = size_letter, hjust=0, vjust=0.5, 
            family = type_letter) +
  # p-value
  geom_text(aes(x = start_fourth_col, label = new_q.value),
            size = size_letter, hjust=0, vjust=0.5, 
            family = type_letter) +
  # Select ticks x axis
  scale_x_continuous(limits = c(start_first_col, 5), 
                     breaks = -2:2) + 
  # hide background and axis ticks and text
  theme(axis.title =element_blank(),
        # Remove ticks and labels y
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        # background blank
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
  ) 


