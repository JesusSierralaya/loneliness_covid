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

tbl_descriptive <- DB_longer %>% 
  tbl_summary(by = Time, 
              include = !c(ID_ECS, weights),
              missing = "no",
              type = c(loneliness, neuroticism, extraversion) ~ "continuous",
              statistic = list(all_continuous() ~ "{mean} ({sd})")
              )%>%
  add_p(
    include = c(loneliness, disability, socialsupport, 
                physicalactivity, depression, !everything()),
    test = list(c(loneliness, disability, socialsupport) ~ "paired.t.test",
                c(physicalactivity, depression) ~ "mcnemar.test"),
    group = ID_ECS, 
   
  ) %>% 
  bold_labels()
# Remove missing values from the table
tbl_descriptive[1]$table_body <-
  tbl_descriptive[1]$table_body %>%
  mutate(
    across(c(stat_1, stat_2), ~gsub("^0.*", "",.)),
    across(c(stat_1, stat_2), ~gsub("^NA.*", "",.)),
  )