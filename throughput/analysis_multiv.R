# ··············································································
# FILE NAME:   analysis_multiv.R
# DESCRIPTION: Analysis multivariate
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        17/08/2022
# 
# ·············································································· 

## ---- INCLUDES: --------------------------------------------------------------

library(here)
library(tidyverse)
library(labelled)
library(survey)
library(gtsummary)

## ---- LOAD: -----------------------------------------------------------------

load(
  here("dat","db_pre_post.Rda")
)

load(
  here("dat","db_longer_NA.Rda")
)

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

tbl_univ
