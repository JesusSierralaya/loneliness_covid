# ··············································································
# FILE NAME:   import_clean.R
# DESCRIPTION: Importing and clean of the data
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        17/08/2022
# 
# ·············································································· 

## ---- INCLUDES: --------------------------------------------------------------

library(tidyverse)
library(haven)
library(rmarkdown)
library(psych)
library(labelled)
library(readstata13)
library(gt)

## ---- PATHS: -----------------------------------------------------------------

# Base directory
BASE_DIR <- "~/../../UAM"

# Path master directory
DB_PATH_MASTER <- file.path(BASE_DIR, 
                            "marta.miret@uam.es - Bases de datos maestras Edad con Salud")

# File pre
DB_FILE_PRE <- file.path(DB_PATH_MASTER, 
                         "Ola_3/Cohorte_2019", "rawdata_c2019w1.dta") 
# File post
DB_FILE_POST <- file.path(DB_PATH_MASTER, 
                          "Subestudio_COVID", "Edad_con_salud_Fichero_Completo.dta")
# File weights
DB_FILE_WEIGHTS <- file.path(DB_PATH_MASTER, 
                             "Ola_3/Cohorte_2019/Pesos", "pesos_norm.dta")

# Covariates outcomes path
DB_PATH_OUTCOMES <- file.path(BASE_DIR, 
                              "marta.miret@uam.es - Documentacion Edad con Salud")
DB_PATH_OUTCOMES_PRE <- file.path(DB_PATH_OUTCOMES, 
                                  "Edad con salud - Ola 3/Outcomes/Cohorte 2019/Outcome datasets")
DB_PATH_OUTCOMES_POST <- file.path(DB_PATH_OUTCOMES, 
                                   "Edad con salud - Subestudio COVID/Outcomes/Outcome datasets")

# File Disability pre
DB_OUTCOME_PRE_DISABILITY <- file.path(DB_PATH_OUTCOMES_PRE,
                                       "Outcome_disability.dta")
# File Disability post
DB_OUTCOME_POST_DISABILITY <- file.path(DB_PATH_OUTCOMES_POST,
                                        "Outcome_disability.dta")
# File Living Status post
DB_OUTCOME_POST_LIVING_STATUS <- file.path(DB_PATH_OUTCOMES_POST,
                                           "Outcome_social interactions_Subestudio_covid.dta")
# File Personality pre
DB_OUTCOME_PRE_PERSONALITY <- file.path(DB_PATH_OUTCOMES_PRE,
                                        "Outcome_EPQR-A.dta")
# File Physical activity post
DB_OUTCOME_POST_PHYSICAL <- file.path(DB_PATH_OUTCOMES_POST,
                                      "Outcome_physical_activity.dta")
# File Resilience post
DB_OUTCOME_POST_RESILIENCE <- file.path(DB_PATH_OUTCOMES_POST,
                                        "Outcome_brief resilience scale_Subestudio_covid.dta")
# File depression pre
DB_OUTCOME_PRE_DEPRESSION <- file.path(DB_PATH_OUTCOMES_PRE,
                                       "Outcome_depression_ICD10.dta")
# File anxiety pre
DB_OUTCOME_PRE_ANXIETY <- file.path(DB_PATH_OUTCOMES_PRE,
                                    "Outcome_anxietyDSM4.dta")
# File panic pre
DB_OUTCOME_PRE_PANIC <- file.path(DB_PATH_OUTCOMES_PRE,
                                  "Outcome_panic.dta")
# File depression post
DB_OUTCOME_POST_DEPRESSION <- file.path(DB_PATH_OUTCOMES_POST,
                                        "Outcome_depression_ICD10.dta")

# File economic post
DB_OUTCOME_POST_ECONOMIC <- file.path(DB_PATH_OUTCOMES_POST,
                                      "Outcome_economic.dta")
# Weights
DB_FILE_WEIGHTS <- file.path(DB_PATH_MASTER, "Ola_3/Cohorte_2019/Pesos", "pesos_norm.dta")

# Material deprivation. Added 11/7/2022
DB_OUTCOME_MAT_DEPRIV <- file.path(DB_PATH_OUTCOMES_PRE,
                                "Outcome_materialdeprivation.dta")

# File Physical activity post. Added 20/7/2022
DB_OUTCOME_PRE_PHYSICAL <- file.path(DB_PATH_OUTCOMES_PRE,
                                     "Outcome_physicalactivity.dta")

# File health status post. Added 20/7/2022
DB_OUTCOME_PRE_HEALTH <- file.path(DB_PATH_OUTCOMES_PRE,
                                   "Outcome_healthstatus.dta")

# Check that there is the correct files and outcomes
# ls(pat = "DB_FILE"); ls(pat = "DB_OUTCOME")


## ---- SELECT COLUMNS: --------------------------------------------------------

# Data base pre

col_pre <- c("ID_ECS", # Id 
             "subsample_pre", # Subsample pre lockdown
             "q6351_companion", # Loneliness item 1
             "q6352_leftout", # Loneliness item 2
             "q6353_isolated", # Loneliness item 3
             "q1009_sex", # Sex
             "q1011_age", # Age
             "q1016_highest", # Educational level
             "q1012_mar_stat", # Marital status
             "q7008d_cantril", 
             "q6320_close", # soc supp punt original pre
             "q6310_help_neig", # soc supp punt original pre
             "q6330_concern" # soc supp punt original pre
)

# Data base post

col_post <- c("ID_ECS", # Id
              "ESTADO_ENTREVISTA", # Interview post lockdown
              "SOLO7_1", # Loneliness item 1
              "SOLO7_2", # Loneliness item 2
              "SOLO7_3", # Loneliness item 3
              "SOLO3", # Personal relationship
              "SOLO2", # Virtual contact. Add 11/7/2022
              "ECON5", # Unemployment by covid. Add 20/7/2022
              "SOLO9A", # soc supp punt original post
              "SOLO9C", # soc supp punt original post
              "SOLO9B" # soc supp punt original post
)

# Disability (Physical health)
col_disability <- c("ID_ECS", # Id
                    "whodas12" # Variable outcome
)

# Living status (living or not alone)
col_living_status <- c("ID_ECS", # Id
                       "living_alone"
)

# Personality
col_personality <- c("ID_ECS", # Id
                     "neuroticism",
                     "extraversion")
# Physical activity 
col_physical <- c("ID_ECS", # Id
                  "physical")   

# Resilience 
col_resilience <- c("ID_ECS", # Id
                    "resilience_scale")

# Depression pre
col_depression_pre <- c("ID_ECS", # Id
                        "depression_12m")
# Anxiety pre
col_anxiety <- c("ID_ECS", # Id
                 "GAD12m")

# Panic pre
col_panic <- c("ID_ECS", # Id
               "PAN12m")

# Depression post
col_depression_post <- c("ID_ECS", # Id
                         "depression_30d")
# Economic post
col_economic_post <- c("ID_ECS", # Id
                       "economy")

# V.O pre: material deprivation
col_mat_deprivation <- c("ID_ECS", # Id
                         "material")

# V.O pre: physical activity
col_physical_pre <- c("ID_ECS", # Id
                      "physical")   

# v.o pre: health status
col_health_pre <-  c("ID_ECS", # Id
                     "health")

# Check all cols selected
# ls(pat = "col_")

## ---- LOAD: -----------------------------------------------------------------

# Load file pre
db_pre <- DB_FILE_PRE |> 
  read_dta(col_select = all_of(col_pre))

# Load file post
db_post <- DB_FILE_POST |> 
  read.dta13(select.cols = col_post)

# Load disability pre
db_disability_pre <- DB_OUTCOME_PRE_DISABILITY |> 
  read_dta(col_select = col_disability) |> 
  rename(whodas12_pre = whodas12)

# Load disability post
db_disability_post <- DB_OUTCOME_POST_DISABILITY |> 
  read_dta(col_select = col_disability) |> 
  rename(whodas12_post = whodas12)

# Load living status post
db_living_status_post <- DB_OUTCOME_POST_LIVING_STATUS |> 
  read_dta(col_select = col_living_status) |> 
  rename(living_alone_post = living_alone)

# Load personality pre
db_personality <- DB_OUTCOME_PRE_PERSONALITY |> 
  read_dta(col_select = col_personality)

# Load physical post
db_physical_post <- DB_OUTCOME_POST_PHYSICAL |> 
  read_dta(col_select = col_physical) |> 
  rename(physical_post = physical)

# Load resilience post
db_resilience_post <- DB_OUTCOME_POST_RESILIENCE |> 
  read_dta(col_select = col_resilience) |> 
  rename(resilience_scale_post = resilience_scale)

# Load depression pre
db_depression_pre <- DB_OUTCOME_PRE_DEPRESSION |> 
  read_dta(col_select = col_depression_pre) |> 
  rename(depression_12m_pre = depression_12m)

# Load panic pre
db_panic_pre <- DB_OUTCOME_PRE_PANIC |> 
  read_dta(col_select = col_panic) |> 
  rename(PAN12m_pre = PAN12m)

# Load depression post
db_depression_post <- DB_OUTCOME_POST_DEPRESSION |> 
  read_dta(col_select = col_depression_post) |> 
  rename(depression_30d_post = depression_30d)

# Load economic post
db_economic_post <- DB_OUTCOME_POST_ECONOMIC |> 
  read_dta(col_select = col_economic_post) |> 
  rename(economy_post = economy)

# Load weights
db_weights <- DB_FILE_WEIGHTS |> 
  read_dta(col_select = c("ID_ECS", "wfinal_norm"))

# Load material deprivation
# Path: DB_FILE_MAT_DEPRIV
# Columns: col_mat_deprivation
db_mat_deprivation <- DB_OUTCOME_MAT_DEPRIV |> 
  read_dta(col_select = all_of(col_mat_deprivation))

# Load physical activity pre
# Path: DB_OUTCOME_PRE_PHYSICAL
# Columns: col_physical_pre
db_physical_pre <- DB_OUTCOME_PRE_PHYSICAL |> 
  read_dta(col_select = all_of(col_physical_pre)) |> 
  rename(physical_pre = physical)

# Load health status pre
# Path: DB_OUTCOME_PRE_HEALTH
# Columns: col_health_pre
db_health_pre <- DB_OUTCOME_PRE_HEALTH |> 
  read_dta(col_select = all_of(col_health_pre)) |> 
  rename(health_pre = health)

# Check all data load
# ls(pat = "db_")

## ---- MERGE: -----------------------------------------------------------------

db_pre_post <- full_join(db_pre, db_post, by = "ID_ECS") |>
  filter(subsample_pre == 1 & ESTADO_ENTREVISTA == 1) |>
  left_join(db_disability_pre, by = "ID_ECS") |> # disability_pre
  left_join(db_disability_post, by = "ID_ECS") |>  # disability_post
  left_join(db_living_status_post, by = "ID_ECS") |> # soc_supp_post
  left_join(db_personality, by = "ID_ECS") |> # personality_pre
  left_join(db_physical_post, by = "ID_ECS") |>  # physical_post
  left_join(db_resilience_post, by = "ID_ECS") |> # resilience_post
  left_join(db_depression_pre, by = "ID_ECS") |> # depression_pre
  left_join(db_panic_pre, by = "ID_ECS") |> # panic_pre
  left_join(db_depression_post, by = "ID_ECS") |> # depression_post
  left_join(db_economic_post, by = "ID_ECS") |>  # depression_post 
  left_join(db_weights, by = "ID_ECS") |>   # weights 
  left_join(db_mat_deprivation, by = "ID_ECS") |> # mat deprivation
  left_join(db_physical_pre, by = "ID_ECS") |> # mat deprivation 
  left_join(db_health_pre, by = "ID_ECS")

## ---- BRIEFLY DESCRIPTIVES: --------------------------------------------------

# # Version to print
# describe(db_pre_post)[-(c(1,9,10)), c("n", "mean", "min", "max")] |>
#   rownames_to_column() |> 
#   gt() |> 
#   fmt_number(
#     columns = c("mean", "min", "max"),
#     decimals = 1,
#     use_seps = FALSE
#   ) |>  
#   tab_header(
#     title = md("Briefly descriptives **BEFORE transformation**"),
#     subtitle = md("*Loneliness covid study*")
#   )

## ---- TREATMENT OF OUTLIERS TO NA --------------------------------------------

# SOLO3: Live alone. Range 1:3
# Check frequency
# addmargins(table(db_pre_post$SOLO3, useNA = "always"))
# Replace >3 to NA 
db_pre_post$SOLO3 <- db_pre_post$SOLO3 |> na_if(9)
# Recheck
# addmargins(table(db_pre_post$SOLO3, useNA = "always"))

# ECON5: recode 9 to missing
db_pre_post <- db_pre_post |> 
  mutate(ECON5 = if_else(ECON5 == 9, NA_real_, ECON5))



## ---- TREATMENT OF NA --------------------------------------------------------

# Check the NA for each variable
# Version to print
# db_pre_post |> 
#   summarise(across(.fns = ~sum(is.na(.)))) |> 
#   pivot_longer(everything()) |> 
#   gt()  |>  
#   tab_header(
#     title = md("NA **BEFORE transformation**"),
#     subtitle = md("*Loneliness covid study*")
#   )

# Drop NA in the criterion variable loneliness pre and post
db_pre_post <- db_pre_post |> 
  drop_na(any_of(c(
    "q6351_companion",
    "q6352_leftout",
    "q6353_isolated",
    "SOLO7_1",
    "SOLO7_2",
    "SOLO7_3"
  )))



## ---- TRANSFORMATION OF VARIABLES --------------------------------------------

# Loneliness pre and post
db_pre_post <- db_pre_post |> 
  mutate(loneliness_pre = 
           q6351_companion + q6352_leftout + q6353_isolated, 
         loneliness_post = 
           SOLO7_1 + SOLO7_2 + SOLO7_3,
         .keep = "unused") # delete the column no longer need

# Age grouped
db_pre_post <- db_pre_post |> 
  mutate(q1011_age_cat = 
           cut(q1011_age, breaks = c(-Inf, 35, 50, 65 ,Inf),
               labels = c("18-34", "35-49", "50-64", "+65"), right = FALSE)) |> 
  relocate(q1011_age_cat,.after = q1011_age)

# Marital status recat
db_pre_post <- db_pre_post |> 
  mutate(q1012_mar_stat_recat = 
           case_when(
             q1012_mar_stat == 1  ~ 1,
             q1012_mar_stat == 2 | q1012_mar_stat == 3 ~ 2, 
             q1012_mar_stat == 4 | q1012_mar_stat == 5 ~ 3),
         .keep = "unused"
  ) |> 
  relocate(q1012_mar_stat_recat,.after = q1011_age_cat)

# Education level recat
db_pre_post <- db_pre_post |> 
  mutate(q1016_highest_recat =
           case_when(
             q1016_highest <= 1 ~ 1, 
             q1016_highest == 2 ~ 2, 
             q1016_highest == 3 | q1016_highest == 4 ~ 3,
             q1016_highest >= 5 ~ 4)      
         , .keep = "unused"
  ) |> 
  relocate(q1016_highest_recat,.after = q1012_mar_stat_recat)

# Social support
db_pre_post <- db_pre_post |> 
  mutate(q6320_close = q6320_close |> unclass(),
         q6310_help_neig = q6310_help_neig |> unclass(),
         q6330_concern = q6330_concern |> unclass(),
         SOLO9A  = SOLO9A  |> unclass(),
         SOLO9C = SOLO9C |> unclass(),
         SOLO9B = SOLO9B |> unclass()) |> 
  mutate(q6320_close = if_else(q6320_close > 887, NA_real_, q6320_close),
         q6310_help_neig = if_else(q6310_help_neig > 887, NA_real_, q6310_help_neig),
         q6330_concern = if_else(q6330_concern > 887, NA_real_, q6330_concern),
         SOLO9A  = if_else(SOLO9A  > 887, NA_real_, SOLO9A ),
         SOLO9C = if_else(SOLO9C > 887, NA_real_, SOLO9C),
         SOLO9B = if_else(SOLO9B > 887, NA_real_, SOLO9B)) |> 
  mutate(social_support_pre_original = 
           q6320_close + (6 - q6310_help_neig) + (6 - q6330_concern), 
         social_support_post_original = 
           SOLO9A  + (6 - SOLO9C) + (6 - SOLO9B),
         .keep = "unused") # delete the column no longer need

# Drop some items that will no used

db_pre_post <- db_pre_post |>
  select(-c(subsample_pre, ESTADO_ENTREVISTA))

# ---- RECHECK DESCRIPTIVES AND NA ---------------------------------------------

# describe(db_pre_post)[-(c(1,9,10)), c("n", "mean", "min", "max")] |>
#   rownames_to_column() |> 
#   gt() |> 
#   fmt_number(
#     columns = c("mean", "min", "max"),
#     decimals = 1,
#     use_seps = FALSE
#   ) |>  
#   tab_header(
#     title = md("Briefly descriptives **AFTER transformation**"),
#     subtitle = md("*Loneliness covid study*")
#   )

# Check the NA for each variable
# Version to print
# db_pre_post |> 
#   summarise(across(.fns = ~sum(is.na(.)))) |> 
#   pivot_longer(everything()) |> 
#   gt()  |>  
#   tab_header(
#     title = md("NA **AFTER transformation**"),
#     subtitle = md("*Loneliness covid study*")
#   )

# Codebook 
# db_pre_post |> look_for(details = "full") |> 
#   select(-starts_with("na_")) |> gt()


# ---- DATABASE: db_longer_NA ---------------------------------------------

# Created for descriptives table

db_longer_NA <- db_pre_post |> 
  select(ID_ECS,
         q1011_age, 
         q1011_age_cat,
         q1009_sex, 
         q1012_mar_stat_recat,
         q1016_highest_recat,
         SOLO3,
         loneliness_pre, loneliness_post,
         resilience_scale_post,
         whodas12_pre, whodas12_post,
         # social_support_pre, social_support_post,
         social_support_pre_original, 
         social_support_post_original,
         living_alone_post,
         neuroticism, extraversion,
         physical_post,
         depression_12m_pre, depression_30d_post,
         #GAD12m_pre, 
         PAN12m_pre,
         economy_post,
         # loneliness_pre_cat, loneliness_post_cat,
         wfinal_norm
         # FALTARIA los nuevos
  ) |> 
  pivot_longer(
    cols = c("loneliness_pre","loneliness_post"),
    names_to = "Assessment",
    names_prefix = "loneliness_",
    values_to = "Loneliness"
  )|> 
  # Transform to NA if don't correspond with the assessment
  mutate(
    # Age
    q1011_age = as.numeric(q1011_age),
    q1011_age = if_else(Assessment == "post", NA_real_, q1011_age),
    # Age cat
    q1011_age_cat = as.numeric(q1011_age_cat),
    q1011_age_cat = if_else(Assessment == "post", NA_real_, q1011_age_cat),
    # sex
    q1009_sex = as.numeric(q1009_sex),
    q1009_sex =if_else(q1009_sex == 1, 0, 1),
    q1009_sex = if_else(Assessment == "post", NA_real_, q1009_sex),
    # Marital status recat
    q1012_mar_stat_recat = as.numeric(q1012_mar_stat_recat),
    q1012_mar_stat_recat = if_else(Assessment == "post", NA_real_, q1012_mar_stat_recat),
    # Ed level
    q1016_highest_recat = as.numeric(q1016_highest_recat),
    q1016_highest_recat = if_else(Assessment == "post", NA_real_, q1016_highest_recat),
    # solo 
    SOLO3 = as.numeric(SOLO3),
    SOLO3 = if_else(Assessment == "pre", NA_real_, SOLO3),
    # Resilience
    resilience_scale_post = 
      if_else(Assessment == "pre", NA_real_, resilience_scale_post),
    # Loneliness pivot
    Assessment = Assessment |> fct_relevel("pre"),
    # whodas12
    whodas12 = if_else(
      Assessment == "pre", whodas12_pre, whodas12_post
    ),
    # # social support
    social_support = if_else(
      Assessment == "pre", social_support_pre_original, social_support_post_original
    ),
    # living alone
    living_alone_post = as.numeric(living_alone_post),
    living_alone_post = if_else(Assessment == "pre", NA_real_, living_alone_post),
    # neuroticism
    neuroticism = 
      if_else(Assessment == "post", NA_real_, neuroticism),
    # extraversion
    extraversion = 
      if_else(Assessment == "post", NA_real_, extraversion),
    # physical_post
    physical_post = as.numeric(physical_post),
    physical_post = if_else(Assessment == "pre", NA_real_, physical_post),
    # depression
    depression = if_else(
      Assessment == "pre", depression_12m_pre, depression_30d_post
    ),
    depression = as.numeric(depression),
    # gad 
    # GAD12m_pre = as.numeric(GAD12m_pre),
    # GAD12m_pre = if_else(Assessment == "post", NA_real_, GAD12m_pre),
    # pan
    PAN12m_pre = as.numeric(PAN12m_pre),
    PAN12m_pre = if_else(Assessment == "post", NA_real_, PAN12m_pre),
    # economy_post
    economy_post = as.numeric(economy_post),
    economy_post = if_else(Assessment == "pre", NA_real_, economy_post),
    # loneliness cat
    # loneliness_cat = if_else(Assessment == "pre", loneliness_pre_cat, loneliness_post_cat),
  ) |> 
  select(-c(whodas12_pre, whodas12_post,
            social_support_pre_original, social_support_post_original,
            depression_12m_pre, depression_30d_post#,
            # loneliness_pre_cat, loneliness_post_cat
  ))

# Label db_longer_NA
# Age
var_label(db_longer_NA$q1011_age) <- "Age"
# Sex
var_label(db_longer_NA$q1009_sex) <- "Sex (female)"
# Resilience
var_label(db_longer_NA$resilience_scale_post) <-"Resilience post"
# living alone
var_label(db_longer_NA$living_alone_post) <-"Living alone"
# neuroticism
var_label(db_longer_NA$neuroticism) <-"Neuroticism"
# extraversion
var_label(db_longer_NA$extraversion) <-"Extraversion"
# gad
# var_label(db_longer_NA$GAD12m_pre) <-"Anxiety"
# panic
var_label(db_longer_NA$PAN12m_pre) <-"Panic"
# economic
var_label(db_longer_NA$economy_post) <-"Economy worsened"
# disability
var_label(db_longer_NA$whodas12) <-"Disability"
# social support
var_label(db_longer_NA$social_support) <-"Social support"
# depression
var_label(db_longer_NA$depression) <- "Depression"
# Loneliness grouped (cutoff 4) 
# var_label(db_longer_NA$loneliness_cat) <- "Loneliness grouped (cutoff 4)"


# Labels with mutate
db_longer_NA <- db_longer_NA |> 
  mutate(
    # Assessment
    Assessment = Assessment |> recode(
      pre = "Pre-confinement", post = "Post-confinement"),
    
    # Age grouped 
    q1011_age_cat = q1011_age_cat |> var_label( ) <- "Age grouped",
    q1011_age_cat = q1011_age_cat |> val_label(1) <- "Between 18-34",
    q1011_age_cat = q1011_age_cat |> val_label(2) <- "Between 35-49",
    q1011_age_cat = q1011_age_cat |> val_label(3) <- "Between 50-64",
    q1011_age_cat = q1011_age_cat |> val_label(4) <- "Older than 65",
    q1011_age_cat = q1011_age_cat |> to_factor( ),
    
    # Marital status
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      var_label() <- "Marital status",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      val_label(1) <- "Never married",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      val_label(2) <- "Married/ Law-partner",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      val_label(3) <- "Separated/ Divorced/ Widowed ",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> to_factor( ),
    
    # Educational level
    q1016_highest_recat = q1016_highest_recat |> 
      var_label() <- "Education level",
    q1016_highest_recat = q1016_highest_recat |>
      val_label(1) <- "Less than primary",
    q1016_highest_recat = q1016_highest_recat |> 
      val_label(2) <- "Primary",
    q1016_highest_recat = q1016_highest_recat |> 
      val_label(3) <- "Secondary",
    q1016_highest_recat = q1016_highest_recat |> 
      val_label(4) <- "Tertiary",
    q1016_highest_recat = q1016_highest_recat |> to_factor( ),
    
    # Social relationships changes
    SOLO3 = SOLO3 |> var_label( ) <- "Social relationships changes",
    SOLO3 = SOLO3 |> val_label(1) <- "Have improved",
    SOLO3 = SOLO3 |> val_label(2) <- "Have worsened",
    SOLO3 = SOLO3 |> val_label(3) <- "Have had no effect",
    SOLO3 = SOLO3 |> to_factor( ),
    
    # Physical activity
    physical_post = physical_post |> var_label( ) <- "Physical activity",
    physical_post = physical_post |> val_label(0) <- "High",
    physical_post = physical_post |> val_label(1) <- "Moderate",
    physical_post = physical_post |> val_label(2) <- "Low",
    physical_post = physical_post |> to_factor( ),
  )

# # Codebook for print
# db_longer_NA |> look_for(details = "full") |> 
#   select(-starts_with("na_")) |> gt()

# ---- DATABASE: db_longer ---------------------------------------------

# Created for descriptives graphs
db_longer <- db_pre_post |> 
  select(ID_ECS,
         q1011_age, 
         q1011_age_cat,
         q1009_sex, 
         q1012_mar_stat_recat,
         q1016_highest_recat,
         SOLO3,
         loneliness_pre, loneliness_post,
         resilience_scale_post,
         whodas12_pre, whodas12_post,
         # social_support_pre, social_support_post,
         living_alone_post,
         neuroticism, extraversion,
         physical_post,
         depression_12m_pre, depression_30d_post,
         #GAD12m_pre, 
         PAN12m_pre,
         economy_post,
         # loneliness_pre_cat, loneliness_post_cat,
         SOLO2, # Virtual contact
         ECON5, # Unemployment
         wfinal_norm,
         material,
         physical_pre,
         health_pre,
         q7008d_cantril,
         social_support_pre_original,
         social_support_post_original
  ) |> 
  pivot_longer(
    cols = c("loneliness_pre","loneliness_post"),
    names_to = "Assessment",
    names_prefix = "loneliness_",
    values_to = "Loneliness"
  )|> 
  mutate(
    whodas12 = if_else(
      Assessment == "pre", whodas12_pre, whodas12_post
    ),
    # depression
    depression = if_else(
      Assessment == "pre", depression_12m_pre, depression_30d_post
    )
  ) 

# Label Pivot for descriptives

# Age
var_label(db_longer$q1011_age) <- "Age"
# Sex
var_label(db_longer$q1009_sex) <- "Sex (female)"
# Resilience
var_label(db_longer$resilience_scale_post) <-"Resilience post"
# living alone
var_label(db_longer$living_alone_post) <-"Living alone"
# neuroticism
var_label(db_longer$neuroticism) <-"Neuroticism"
# extraversion
var_label(db_longer$extraversion) <-"Extraversion"
# panic
var_label(db_longer$PAN12m_pre) <-"Panic"
# economic
var_label(db_longer$economy_post) <-"Economy worsened"
# disability
var_label(db_longer$whodas12) <-"Disability"

# depression
var_label(db_longer$depression) <- "Depression"

# Labels with mutate
db_longer <- db_longer |> 
  mutate(
    # Assessment
    Assessment = Assessment |> recode(
      pre = "Pre-confinement", post = "Post-confinement"),

    # Marital status
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      var_label() <- "Marital status",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      val_label(1) <- "Never married",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      val_label(2) <- "Married/ Law-partner",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> 
      val_label(3) <- "Separated/ Divorced/ Widowed ",
    q1012_mar_stat_recat = q1012_mar_stat_recat |> to_factor( ),
    
    # Educational level
    q1016_highest_recat = q1016_highest_recat |> 
      var_label() <- "Education level",
    q1016_highest_recat = q1016_highest_recat |>
      val_label(1) <- "Less than primary",
    q1016_highest_recat = q1016_highest_recat |> 
      val_label(2) <- "Primary",
    q1016_highest_recat = q1016_highest_recat |> 
      val_label(3) <- "Secondary",
    q1016_highest_recat = q1016_highest_recat |> 
      val_label(4) <- "Tertiary",
    q1016_highest_recat = q1016_highest_recat |> to_factor( ),
    
    # Social relationships changes
    SOLO3 = SOLO3 |> var_label( ) <- "Social relationships changes",
    SOLO3 = SOLO3 |> val_label(1) <- "Have improved",
    SOLO3 = SOLO3 |> val_label(2) <- "Have worsened",
    SOLO3 = SOLO3 |> val_label(3) <- "Have had no effect",
    SOLO3 = SOLO3 |> to_factor( ),
    
    # Physical activity
    physical_post = physical_post |> var_label( ) <- "Physical activity",
    physical_post = physical_post |> val_label(0) <- "High",
    physical_post = physical_post |> val_label(1) <- "Moderate",
    physical_post = physical_post |> val_label(2) <- "Low",
    physical_post = physical_post |> to_factor( ),
  )

# depression pre post
var_label(db_longer$depression_12m_pre) <- "Depression pre 12 month"
var_label(db_longer$depression_30d_post) <- "Depression post 30 days"
val_labels(db_longer$depression_30d_post) <- 
  c("No" = 0, "Yes" = 1)

# panic
val_labels(db_longer$PAN12m_pre) <- 
  c("No" = 0, "Yes" = 1)

# sex
val_label(db_longer$q1009_sex, 2) <- "Female"

# # Codebook for print
# db_longer |> look_for(details = "full") |> 
#   select(-starts_with("na_")) |> gt()

# ---- DATABASE: db_pre_post ---------------------------------------------

# The db is created but we add the labels

var_label(db_pre_post$q1009_sex) <- "Sex"
var_label(db_pre_post$q1011_age) <- "Age"
var_label(db_pre_post$q1011_age_cat) <- "Age grouped"
var_label(db_pre_post$q1012_mar_stat_recat) <- "Marital status"
var_label(db_pre_post$q1016_highest_recat) <- "Education level"
var_label(db_pre_post$SOLO3) <- "Social relationships changes"
var_label(db_pre_post$whodas12_pre) <- "Disability pre"
var_label(db_pre_post$whodas12_post) <- "Disability post"
var_label(db_pre_post$living_alone_post) <- "Living alone post"
var_label(db_pre_post$depression_12m_pre) <- "Depression pre 12 month"
var_label(db_pre_post$depression_30d_post) <- "Depression post 30 days"
var_label(db_pre_post$loneliness_pre) <- "Loneliness pre"
var_label(db_pre_post$loneliness_post) <- "Loneliness post"
# Check values labels
# val_labels(db_pre_post$q1009_sex)
# Change values labels
val_label(db_pre_post$q1009_sex, 2) <- "Female"
# Age
val_label(db_pre_post$q1011_age, 888) <- NULL
# Marital status
val_labels(db_pre_post$q1012_mar_stat_recat) <- 
  c("Single" = 1, "Married/ Partnership" = 2, "Separated/ Widowed" = 3)
# Education level
val_labels(db_pre_post$q1016_highest_recat) <- 
  c("Less than primary" = 1, 
    "Primary" = 2, 
    "Secondary" = 3,
    "Tertiary"  = 4
  )
# Social relationship
val_labels(db_pre_post$SOLO3) <- 
  c("Have improved" = 1, 
    "Have worsened" = 2, 
    "Have had no effect" = 3
  )

# Living alone
val_labels(db_pre_post$living_alone_post) <- 
  c("No" = 0, 
    "Yes" = 1
  )

# Panic
val_labels(db_pre_post$PAN12m_pre) <- 
  c("No" = 0, 
    "Yes" = 1
  )

# DEPRESSION
val_labels(db_pre_post$depression_30d_post) <- 
  c("No" = 0, 
    "Yes" = 1
  )

# Codebook for print
# db_pre_post |> look_for(details = "full") |> 
#   select(-starts_with("na_")) |> gt()

# # ---- SAVE DATABASES ---------------------------------------------
# 
# save(db_longer_NA, file = "dat/db_longer_NA.Rda")
# save(db_longer, file = "dat/db_longer.Rda")
# save(db_pre_post, file = "dat/db_pre_post.Rda")
