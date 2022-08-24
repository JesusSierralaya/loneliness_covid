# ··············································································
# FILE NAME:   descriptives.R
# DESCRIPTION: descriptives table 1
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        23/08/2022
# 
# ·············································································· 

## ---- INCLUDES: --------------------------------------------------------------

# library(here)
library(gtsummary)

## ---- TABLE 1: --------------------------------------------------------------

# # Load
# load(
#   here("dat","db_longer_NA.Rda")
# )

# Table with tbl summary
tbl_descriptive <- db_longer_NA %>%
  tbl_summary(
    missing = "no",
    digits = all_continuous() ~ 2,
    by = Assessment, include = -c(ID_ECS, wfinal_norm),
    type = c(Loneliness, neuroticism, extraversion) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
  ) %>%
  add_p(
    test = list(c(Loneliness, whodas12, social_support) ~ "paired.t.test",
                c(depression) ~ "mcnemar.test"),
    group = ID_ECS
  )

# Remove missing values from the table
tbl_descriptive[1]$table_body <-
  tbl_descriptive[1]$table_body %>%
  mutate(
    across(c(stat_1, stat_2), ~gsub("^0.*", "",.)),
    across(c(stat_1, stat_2), ~gsub("^NA.*", "",.)),
  )

