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
# tbl_univ <-
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

tbl_multi <- fit_multi %>% 
  tbl_regression(
    pvalue_fun = purrr::partial(style_sigfig, digits = 3)
  ) |>
  bold_p(t = 0.05) |>
  add_vif()|>
  add_global_p(keep = TRUE)

# Plot
plot_multi <-
  fit_multi %>% 
  plot_models(show.values = TRUE, 
                            show.legend = FALSE, 
                            colors = "Dark2",
                            axis.labels = rev(c(
                              "Age (18-34)",
                              "Age (35-49)",
                              "Age (+65)", 
                              "Sex (Female)",
                              "Education level (No formal)",
                              "Education level (Secundary)",
                              "Education level (Primary)",
                              "Marital status (Divorced, separated or widowed)",
                              "Marital status (Single)",
                              "Virtual contact (Once a week)",
                              "Virtual contact (Less than once a week)",
                              "Virtual contact (Never)",
                              "Social changes (Improved)",
                              "Social changes (Worsened)",
                              "Economy worsened",
                              "Unemployment",
                              "Depression 12 months (Before)",
                              "Depression 30 days (During)",
                              "Neuroticism",
                              "Extraversion",
                              "Disability (Before)"
                            ))) + 
  font_size(labels.y = 10)

# plot_multi <- fit_multi |> plot_models(show.values = TRUE, axis.labels = c(

#   "Social relationships changes (Improved)",
#   "Social relationships changes (Worsened)",
#   "Economic situation worsened due to COVID-19",
#   "Neuroticism",
#   "Extraversion",
#   "Disability (Before)",
#   "Depression 12 months (Before)",
#   "Depression 30 days (During)"
# )|> rev(), show.legend = FALSE, colors = "Dark2") +
#   font_size(title =20, labels.y = 10, axis_title.x = 15)