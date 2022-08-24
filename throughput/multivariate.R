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

## ---- LOAD: -----------------------------------------------------------------

# load(
#   here("dat","db_pre_post.Rda")
# )

## ---- UNIVARIATE: ------------------------------------------------------------

# Select the reference group
db_pre_post <- db_pre_post |>
  mutate(
    q1011_age_cat = q1011_age_cat |> fct_relevel("50-64"),
    q1012_mar_stat_recat = q1012_mar_stat_recat |> to_factor() |>
      fct_relevel("Married/ Partnership"),
    q1016_highest_recat = q1016_highest_recat |> to_factor() |> fct_relevel("Tertiary"),
    SOLO3 = SOLO3 |> to_factor() |> fct_relevel("Have had no effect"),
  )

# As factor
db_pre_post <- db_pre_post |>
  haven::as_factor()

# Univariate with Survey design
tbl_univ <- survey::svydesign(
  data = db_pre_post,
  ids = ~ID_ECS,
  weights = db_pre_post$wfinal_norm
) |>
  tbl_uvregression(
    method = survey::svyglm,
    y = loneliness_post,
    formula = "{y} ~ {x} + offset(loneliness_pre)",
    include = -c(q1011_age, ID_ECS, q7008d_cantril,
                 loneliness_pre, SOLO2, ECON5,
                 wfinal_norm),
    label = list(economy_post ~ "Economy worsened due COVID")
  ) |>
  add_global_p(keep = TRUE) |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE, hide_se = TRUE) |>
  bold_p() |>
  italicize_levels()

## ---- MULTIVARIATE: ------------------------------------------------------------

fit_multi <- survey::svydesign(
  data = db_pre_post,
  ids = ~ID_ECS,
  weights = db_pre_post$wfinal_norm
) |>
  svyglm(
    formula = loneliness_post ~ offset(loneliness_pre) #+ 0
    # Fix
    + q1009_sex
    + q1011_age_cat
    + q1012_mar_stat_recat
    + q1016_highest_recat
    # random
    + SOLO3
    + economy_post
    + neuroticism
    + extraversion
    + whodas12_pre
    + depression_12m_pre
    + depression_30d_post
    # ,
    # design  = svy_design
  )

# # Quick review of significant factors
# summary(fit_multi)
#
# Table
tbl_multi <- 
  fit_multi |>
    tbl_regression(
      pvalue_fun = purrr::partial(style_sigfig, digits = 3)
    ) |>
    bold_p(t = 0.05) |>
    add_vif()|>
    add_global_p(keep = TRUE)

# Plot

plot_multi <- fit_multi |> plot_models(show.values = TRUE, axis.labels = c(
  "Sex (Female)",
  "Age (18-34)",
  "Age (35-49)",
  "Age (+65)",
  "Marital status (Single)",
  "Marital status (Separated/ Widowed)",
  "Education level (Less than primary)",
  "Education level (Primary)",
  "Education level (Secondary)",
  "Social relationships changes (Improved)",
  "Social relationships changes (Worsened)",
  "Economic situation worsened due to COVID-19",
  "Neuroticism",
  "Extraversion",
  "Disability (Before)",
  "Depression 12 months (Before)",
  "Depression 30 days (During)"
)|> rev(), show.legend = FALSE, colors = "Dark2") +
  font_size(title =20, labels.y = 10, axis_title.x = 15)