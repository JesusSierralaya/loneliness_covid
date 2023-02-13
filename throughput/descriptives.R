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

# source("throughput/import_clean.R", encoding = 'UTF-8')

## ---- TABLE 1: --------------------------------------------------------------

tbl_descriptive <- DB_longer %>% 
  tbl_summary(by = Time, 
              include = !c(ID_ECS, weights),
              missing = "no",
              type = c(loneliness, neuroticism, extraversion) ~ "continuous",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(age ~ labels_longer[labels_longer == 
                                                 "age",2],
                           sex ~ labels_longer[labels_longer == 
                                                 "sex",2],
                           educlevel ~ labels_longer[labels_longer == 
                                                       "educlevel",2],
                           maritalstatus ~ labels_longer[labels_longer ==
                                                           "maritalstatus",2],
                           virtualcontact ~ labels_longer[labels_longer ==
                                                            "virtualcontact",2],
                           socialchanges ~ labels_longer[labels_longer ==
                                                           "socialchanges",2],
                           economyworsened ~ labels_longer[labels_longer ==
                                                             "economyworsened",2],
                           unemployment ~ labels_longer[labels_longer == 
                                                          "unemployment",2],
                           materialdeprivation ~ labels_longer[labels_longer ==
                                                                 "materialdeprivation",2],
                           livingalone ~ labels_longer[labels_longer == 
                                                         "livingalone",2],
                           physicalactivity ~ labels_longer[labels_longer ==
                                                              "physicalactivity",2],
                           depression ~ labels_longer[labels_longer == 
                                                        "depression",2],
                           neuroticism ~ labels_longer[labels_longer == 
                                                         "neuroticism",2],
                           extraversion ~ labels_longer[labels_longer == 
                                                          "extraversion",2],
                           disability ~ labels_longer[labels_longer == 
                                                        "disability",2],
                           loneliness ~ labels_longer[labels_longer == 
                                                        "loneliness",2],
                           resilience ~ labels_longer[labels_longer == 
                                                        "resilience",2],
                           socialsupport ~ labels_longer[labels_longer ==
                                                           "socialsupport",2],
                           wellbeingcantril ~ labels_longer[labels_longer ==
                                                              "wellbeingcantril",2]
                           )
              )%>%
  add_p(
    include = c(loneliness, disability, socialsupport, 
                physicalactivity, depression, !everything()),
    test = list(c(loneliness, disability, socialsupport) ~ "paired.t.test",
                c(physicalactivity, depression) ~ "mcnemar.test"),
    group = ID_ECS,
    test.args = all_tests("mcnemar.test") ~ list(correct = FALSE)
   
  ) %>% 
  bold_labels()
# Remove missing values from the table
tbl_descriptive[1]$table_body <-
  tbl_descriptive[1]$table_body %>%
  mutate(
    across(c(stat_1, stat_2), ~gsub("^0.*", "",.)),
    across(c(stat_1, stat_2), ~gsub("^NA.*", "",.)),
  )