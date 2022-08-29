# ··············································································
# FILE NAME:   import_clean.R
# DESCRIPTION: Import, clean and transform the database
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        17/08/2022
# 
# ·············································································· 
# NOTES: 
# - First task: all categorical variable in format factor
# - db_* databases that will collapse have the prefix db_*
# - DB_* Final databases have the prefix DB_*
# ·············································································· 

# INCLUDES ---------------------------------------------------------------------

library(tidyverse)
library(haven)
library(rmarkdown)
library(psych)
library(labelled)
library(readstata13)
library(gt)
library(magrittr)
library(paint)

# PARAMETERS -------------------------------------------------------------------

  ## PROGRAM ----------------
  
  # Remove all objects
  rm(list = ls())

  # Prevent the warnings
  options(warn=-1)

  ## PATHS BASES ------------
  
  # Base directory 
  BASE_DIR <- "~/../../UAM"
  
  # Path master directory
  PATH_MASTER <- 
    file.path(BASE_DIR, 
              "marta.miret@uam.es - Bases de datos maestras Edad con Salud")
  # File pre
  PATH_FILE_PRE <- 
    file.path(PATH_MASTER, 
              "Ola_3/Cohorte_2019", "rawdata_c2019w1.dta") 
  # File post
  PATH_FILE_POST <- 
    file.path(PATH_MASTER, 
              "Subestudio_COVID", "Edad_con_salud_Fichero_Completo.dta")
  
  # Covariates outcomes base paths
  
  PATH_OUTCOMES <- 
    file.path(BASE_DIR, 
              "marta.miret@uam.es - Documentacion Edad con Salud")
  PATH_OUTCOMES_PRE <- 
    file.path(PATH_OUTCOMES, 
              "Edad con salud - Ola 3/Outcomes/Cohorte 2019/Outcome datasets")
  PATH_OUTCOMES_POST <- 
    file.path(PATH_OUTCOMES, 
              "Edad con salud - Subestudio COVID/Outcomes/Outcome datasets")

# IMPORT VARIABLES -------------------------------------------------------------

  ## FILTER --------------
  
    ### PATH ---
    
    # PATH_FILE_PRE
    # PATH_FILE_POST
    
    ### READ ---
    
    filter_pre <- PATH_FILE_PRE %>% 
      read_dta(col_select = all_of(c("ID_ECS","subsample_pre"))) 
    
    filter_post <- PATH_FILE_POST %>% 
      read_dta(col_select = all_of(c("ID_ECS","ESTADO_ENTREVISTA"))) 
    
    ### TRANSFORMATION ---
    
    db_filter <- full_join(filter_pre, filter_post, by = "ID_ECS")
  
  ## WEIGHTS --------------
  
    ### PATH ---
    
    PATH_WEIGHTS <- file.path(PATH_MASTER, 
                              "Ola_3/Cohorte_2019/Pesos", "pesos_norm.dta")
    
    ### READ ---
    
    db_weights <- PATH_WEIGHTS |> 
      read_dta(col_select = c("ID_ECS", "wfinal_norm"))
    
    ### TRANSFORMATION ---
    
    db_weights %<>% rename(weights = wfinal_norm) # Change name
  
  ## LONELINESS PRE-POST ----------
  
    ### PATH ---
    
    # PATH_FILE_PRE
    # PATH_FILE_POST
    
    ### READ ---
    
    # read file pre
    loneliness_pre <- PATH_FILE_PRE %>% 
      read_dta(col_select = all_of(c("ID_ECS", # Id 
                                   "q6351_companion", # Loneliness item 1
                                   "q6352_leftout", # Loneliness item 2
                                   "q6353_isolated"))) # Loneliness item 3))
    
    # read file post
    loneliness_post <- PATH_FILE_POST %>% 
      read_dta(col_select = all_of(c("ID_ECS", # Id 
                                     "SOLO7_1", # Loneliness item 1
                                     "SOLO7_2", # Loneliness item 2
                                     "SOLO7_3"))) # Loneliness item 3))
    
    ### TRANSFORMATION ---
    
    # Transformation loneliness pre
    loneliness_pre %<>% 
    mutate(loneliness_pre = 
             q6351_companion + q6352_leftout + q6353_isolated,
           .keep = "unused") # delete the column no longer need
    
    # Transformation loneliness post
    loneliness_post %<>% 
      mutate(loneliness_post = 
               SOLO7_1 + SOLO7_2 + SOLO7_3,
             .keep = "unused") # delete the column no longer need
    
    # Merge
    db_loneliness <- full_join(loneliness_pre, loneliness_post,
                               by = "ID_ECS")
  
  ## SEX -------------------------------------------------------------------------
  
    ### PATH ---
    
    # PATH_FILE_PRE
    
    ### READ ---
    
    db_sex <- PATH_FILE_PRE %>% 
      read.dta13(select.cols = all_of(c("ID_ECS", "q1009_sex")))
    
    ### TRANSFORMATION ---
    
    db_sex %<>% 
      mutate(sex = q1009_sex %>% recode_factor(
        `1` = "Male",
        `2` = "Female"
        ), .keep = "unused")
    
  
  ## AGE -------------------------------------------------------------------------
  
    ### PATH ---
    
    # PATH_FILE_PRE
    
    ### READ ---
    
    db_age <- PATH_FILE_PRE %>% 
      read_dta(col_select = all_of(c("ID_ECS", # Id 
                                     "q1011_age")))
    
    ### TRANSFORMATION ---
    
    db_age %<>% 
      mutate(age = cut(q1011_age, 
                       breaks = c(-Inf, 35, 50, 65 ,Inf),
                       labels = c("18-34", "35-49", "50-64", "+65"), 
                       right = FALSE),
             .keep = "unused")
  
  ## EDUCATIONAL LEVEL -------------------------------------------------------
  
  ### PATH ---
  
  # PATH_FILE_PRE
  
  ### READ ---
  
  db_educ <- PATH_FILE_PRE %>%
    read_dta(col_select = all_of(c("ID_ECS", # Id
                                   "q1016_highest")))
  
  ### TRANSFORMATION ---
  
  db_educ  %<>% 
    mutate(educ_level =
             case_when(
               q1016_highest <= 1 ~ "No formal", 
               q1016_highest == 2 ~ "Primary", 
               q1016_highest == 3 | q1016_highest == 4 ~ "Secundary",
               q1016_highest >= 5 ~ "Tertiary") %>% as_factor()     
           , .keep = "unused"
           ) 
  
  ## MARITAL STATUS PRE -------------------------------------------------------
  
  ### PATH ---
  
  # PATH_FILE_PRE
  
  ### READ ---
  
  db_marital_status <- PATH_FILE_PRE %>%
    read_dta(col_select = all_of(c("ID_ECS", # Id
                                   "q1012_mar_stat")))

  ### TRANSFORMATION ---
  
  db_marital_status  %<>%
    mutate(marital_status = 
             case_when(
               q1012_mar_stat == 1  ~ "Single",
               q1012_mar_stat == 2 | q1012_mar_stat == 3 ~ 
                 "Married or in partnership", 
               q1012_mar_stat == 4 | q1012_mar_stat == 5 ~ 
                 "Divorced, separated or widowed") %>% as_factor()  ,
           .keep = "unused"
    )
  
  ## DISABILITY (WHODAS12) PRE-POST -------------------------------------------
  
  ### PATH ---
  
  PATH_PRE_DISABILITY <- file.path(PATH_OUTCOMES_PRE,
                                   "Outcome_disability.dta")
  PATH_POST_DISABILITY <- file.path(PATH_OUTCOMES_POST,
                                    "Outcome_disability.dta")
  
  ### READ ---
  
  disability_pre <- PATH_PRE_DISABILITY %>%
    read_dta(col_select = all_of(c("ID_ECS", "whodas12"))) %>% 
    rename(disability_pre = whodas12)
  
  disability_post <- PATH_POST_DISABILITY %>%
    read_dta(col_select = all_of(c("ID_ECS", "whodas12"))) %>% 
    rename(disability_post = whodas12)
  
  ### TRANSFORMATION ---
  
  # Merge
  db_disability <- full_join(disability_pre, disability_post,
                             by = "ID_ECS")
  
  ## SOCIAL SUPPORT PRE-POST -------------------------------------------------
  
  ### PATH ---
  
  # PATH_FILE_PRE
  # PATH_FILE_POST
  
  ### READ ---
  
  social_support_pre <- PATH_FILE_PRE %>% 
    read.dta13(select.cols = all_of(c("ID_ECS",
                                   "q6320_close", 
                                   "q6310_help_neig", 
                                   "q6330_concern")))
  
  social_support_post <- PATH_FILE_POST %>% 
    read.dta13(select.cols = all_of(c("ID_ECS", 
                                   "SOLO9A", 
                                   "SOLO9C", 
                                   "SOLO9B")))
  
  ### TRANSFORMATION ---
  
  # +888 to NA
  social_support_pre %<>%
    mutate(q6320_close = 
             if_else(q6320_close > 887, NA_real_, q6320_close),
           q6310_help_neig = 
             if_else(q6310_help_neig > 887, NA_real_, q6310_help_neig),
           q6330_concern = 
             if_else(q6330_concern > 887, NA_real_, q6330_concern)
           )
  
  social_support_post %<>%
    mutate(SOLO9A  = if_else(SOLO9A  > 887, NA_real_, SOLO9A ),
           SOLO9C = if_else(SOLO9C > 887, NA_real_, SOLO9C),
           SOLO9B = if_else(SOLO9B > 887, NA_real_, SOLO9B)
           )

  # Transformation 
  social_support_pre %<>%
    mutate(social_support_pre = 
             q6320_close + (6 - q6310_help_neig) + (6 - q6330_concern), 
           .keep = "unused") # delete the column no longer need
  
  social_support_post %<>%
    mutate(social_support_post = 
             SOLO9A + (6 - SOLO9C) + (6 - SOLO9B), 
           .keep = "unused") # delete the column no longer need

  # Merge
  db_social_support <- full_join(social_support_pre, social_support_post,
                             by = "ID_ECS")
  
  # db_<var> %<>% 
  #   mutate(age = cut(q1011_age, 
  #                    breaks = c(-Inf, 35, 50, 65 ,Inf),
  #                    labels = c("18-34", "35-49", "50-64", "+65"), 
  #                    right = FALSE),
  #          .keep = "unused")
  
  ## SOCIAL CONTACT (SOLO2) POST ----------------------------------------------
  
  ### PATH ---
  
  # PATH_FILE_POST
  
  ### READ ---
  
  db_virtual_contact_post <- PATH_FILE_POST %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "SOLO2")))
  
  ### TRANSFORMATION ---
  
  db_virtual_contact_post %<>% 
    mutate(virtual_contact_post = SOLO2 %>% recode_factor(
      `1` = "Daily",
      `2` = "Once a week",
      `3` = "Less than once a week",
      `4` = "Never"
    ), .keep = "unused")
  
  ## SOCIAL RELATIONSHIP CHANGES (SOLO3) POST ----------------------------------
  
  ### PATH ---
  
  # PATH_FILE_POST
  
  ### READ ---
  
  db_social_changes_post <- PATH_FILE_POST %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "SOLO3")))
  
  ### TRANSFORMATION ---
  
  db_social_changes_post %<>%
    mutate(social_changes_post = SOLO3 %>% recode_factor(
      `1` = "Improved",
      `2` = "Worsened",
      `3` = "No"
    ), .keep = "unused")
  
  ## ECONOMY WORSENED POST ---------------------------------------------------
  
  ### PATH ---
  
  PATH_POST_ECONOMIC <- file.path(PATH_OUTCOMES_POST,
                                    "Outcome_economic.dta")
  
  ### READ ---
  
  db_economy <- PATH_POST_ECONOMIC %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "economy")))
  
  ### TRANSFORMATION ---
  
  db_economy %<>%
    mutate(economy_worsened_post = economy %>% recode_factor(
      `1` = "Yes",
      `0` = "No"
    ), .keep = "unused")
  
  ## UNEMPLOYMENT POST (ECON5) ------------------------------------------------
  
  ### PATH ---
  
  # PATH_FILE_POST
  
  ### READ ---
  
  db_unemployment <- PATH_FILE_POST %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "ECON5")))
  
  ### TRANSFORMATION ---
  
  db_unemployment %<>%
    mutate(unemployment_post = ECON5 %>% recode_factor(
      `1` = "Yes",
      `2` = "No"
    ), .keep = "unused")
  
  ## MATERIAL DEPRIVATION PRE ------------------------------------------------
  
  ### PATH ---
  
  PATH_PRE_MATERIAL <- file.path(PATH_OUTCOMES_PRE,
                                  "Outcome_materialdeprivation.dta")  
  ### READ ---
  
  db_material <- PATH_PRE_MATERIAL %>%
    read.dta13(select.cols = all_of(c("ID_ECS", # Id
                                   "material")))
  
  ### TRANSFORMATION ---
  
  db_material %<>%
    mutate(material_deprivation_pre = material %>% recode_factor(
      `1` = "Yes",
      `0` = "No"
    ), .keep = "unused")
  
  ## LIVING STATUS/ALONE POST -------------------------------------------
  
  ### PATH ---
  
  PATH_POST_LIVING_ALONE <- 
    file.path(PATH_OUTCOMES_POST,
              "Outcome_social interactions_Subestudio_covid.dta")
  
  ### READ ---
  
  db_living_alone_post <- PATH_POST_LIVING_ALONE %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "living_alone")))

  ### TRANSFORMATION ---
  
  db_living_alone_post %<>%
    mutate(living_alone_post = living_alone %>% recode_factor(
      `1` = "Yes",
      `0` = "No"
    ), .keep = "unused")
  
  ## NEUROTICISM PRE ----------------------------------------------------------
  
  ### PATH ---
  
  PATH_PRE_NEUROTICISM <- file.path(PATH_OUTCOMES_PRE, 
                                    "Outcome_EPQR-A.dta")
  
  ### READ ---
  
  db_neuroticism <- PATH_PRE_NEUROTICISM %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "neuroticism")))
  
  ### TRANSFORMATION ---
  
  # none
  
  ## EXTRAVERSION PRE ---------------------------------------------------------
  
  ### PATH ---
  
  PATH_PRE_EXTRAVERSION <- file.path(PATH_OUTCOMES_PRE,
                                       "Outcome_EPQR-A.dta")
  
  ### READ ---
  
  db_extraversion <- PATH_PRE_EXTRAVERSION %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "extraversion")))
  
  ### TRANSFORMATION ---
  
  # none

  ## PHYSICAL ACTIVITY PRE-POST ---------------------------------------------
  
  ### PATH ---
  
  PATH_PRE_PHYSICAL <- file.path(PATH_OUTCOMES_PRE,
                                       "Outcome_physicalactivity.dta")
  PATH_POST_PHYSICAL <- file.path(PATH_OUTCOMES_POST,
                                        "Outcome_physical_activity.dta")
  
  ### READ ---
  
  physical_pre <- PATH_PRE_PHYSICAL %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "physical")))
  
  physical_post <- PATH_POST_PHYSICAL %>%
    read.dta13(select.cols = all_of(c("ID_ECS", "physical")))
  
  ### TRANSFORMATION ---
  
  physical_pre %<>%
    mutate(physical_activity_pre = physical %>% recode_factor(
      `0` = "High",
      `1` = "Moderate",
      `2` = "Low"
    ), .keep = "unused")
  
  physical_post %<>%
    mutate(physical_activity_post = physical %>% recode_factor(
      `0` = "High",
      `1` = "Moderate",
      `2` = "Low"
    ), .keep = "unused")
  
  # Merge
  db_physical_activity <- full_join(physical_pre, physical_post,
                             by = "ID_ECS")
  
  ## RESILIENCE BRS POST ------------------------------------------------------
  
  ### PATH ---
  PATH_POST_RESILIENCE <- 
    file.path(PATH_OUTCOMES_POST,
              "Outcome_brief resilience scale_Subestudio_covid.dta")
  
  ### READ ---
  
  db_resilience_post <- PATH_POST_RESILIENCE %>% 
      read.dta13(select.cols = all_of(c("ID_ECS", "resilience_scale"))) %>% 
      rename(resilience_post = resilience_scale)
  
  ### TRANSFORMATION ---
  
  # none
  
  ## WELLBEING (CANTRIL) PRE --------------------------------------------------
  
    ## PATH ---
  
    # PATH_FILE_PRE
    
    ### READ ---
    
    db_wellbeing_pre <- PATH_FILE_PRE %>%
      read.dta13(select.cols = all_of(c("ID_ECS", # Id
                                     "q7008d_cantril")))
    
    ### TRANSFORMATION ---
    
    db_wellbeing_pre %<>%
      mutate(wellbeing_cantril_pre = 
               if_else(q7008d_cantril > 800, NA_real_, q7008d_cantril),
             .keep = "unused")
  
  ## DEPRESSION PRE-POST ---------------------------------------------
    
    ### PATH ---
    
    PATH_PRE_DEPRESSION <- file.path(PATH_OUTCOMES_PRE,
                                   "Outcome_depression_ICD10.dta")
    PATH_POST_DEPRESSION <- file.path(PATH_OUTCOMES_POST,
                                    "Outcome_depression_ICD10.dta")
    
    ### READ ---
    
    depression_pre <- PATH_PRE_DEPRESSION %>%
      read.dta13(select.cols = all_of(c("ID_ECS", "depression_12m")))
    
    depression_post <- PATH_POST_DEPRESSION %>%
      read.dta13(select.cols = all_of(c("ID_ECS", "depression_30d")))
    
    ### TRANSFORMATION ---
    
    depression_pre %<>%
      mutate(depression_pre = depression_12m %>% recode_factor(
        `0` = "No",
        `1` = "Yes"
      ), .keep = "unused")
    
    depression_post %<>%
      mutate(depression_post = depression_30d %>% recode_factor(
        `0` = "No",
        `1` = "Yes"
      ), .keep = "unused")
    
    # Merge
    db_depression <- full_join(depression_pre, depression_post,
                                      by = "ID_ECS")
    
    
# MERGE ------------------------------------------------------------------------
  
  # Remove all no db_ 
  rm(list=setdiff(ls(), ls(pattern = "db_")))
  
  # Merge
  DB_pre_post <- 
    lapply(ls(pattern="db_"), get) %>% 
    reduce(full_join, by = "ID_ECS")
  
  # Remove all db_
  rm(list=setdiff(ls(), ls(pattern = "DB_")))
  
  # Apply filters and remove NA in loneliness
  DB_pre_post %<>% 
    filter(subsample_pre == 1 & ESTADO_ENTREVISTA == 1) %>% 
    select(-c(subsample_pre, ESTADO_ENTREVISTA)) %>% 
    drop_na(c(loneliness_pre, loneliness_post))
  
  # Relocate columns position
  DB_pre_post %<>% 
    relocate(ID_ECS, weights, age, sex, educ_level, marital_status, 
             virtual_contact_post, social_changes_post, 
             economy_worsened_post, unemployment_post, 
             material_deprivation_pre, living_alone_post, 
             physical_activity_pre, physical_activity_post,
             depression_pre, depression_post,
             neuroticism, extraversion)
  
  # DESCRIBE
  # DB_pre_post_full %>% describe() %>% select(c("n", "mean", "min", "max"))
  DB_pre_post %>% summary()
  # DB_pre_post_full %>% summarytools::dfSummary() %>% print(method = "render") 

  # SHOW data frame
  DB_pre_post %>% paint()
  
# DATABASE LONGER --------------------------------------
  
  # DB_pre_post %>%
  #   select(ID_ECS, sex, loneliness_pre, loneliness_post) %>% 
  #   pivot_longer(cols = -ID_ECS, 
  #                names_to = c("Time", ".value"),
  #                names_pattern = "(\\w+)_(pre|post)"
  #                ) 
  
  # DB_pre_post %>% 
  #   select(ID_ECS, loneliness_pre, loneliness_post) %>% 
  #   pivot_longer(cols = !ID_ECS, 
  #                names_to = "Time",
  #                names_prefix = "loneliness_",
  #                values_to = "Loneliness"
  #                )
  # # Other type
  # DB_pre_post %>% 
  #   select(ID_ECS, loneliness_pre, loneliness_post,
  #          disability_pre, disability_post) %>% 
  #   pivot_longer(cols = !ID_ECS,
  #                names_to = c("Variable", "Time"),
  #                names_sep = "_",
  #                values_to = "Score")
  
  # DB_pre_post %>% 
  #   select(ID_ECS,
  #          pre_loneliness = loneliness_pre,
  #          post_loneliness = loneliness_post,
  #          pre_disability = disability_pre,
  #          post_disability = disability_post,
  #          pre_sex = sex) %>% 
  #   pivot_longer(
  #     cols = !ID_ECS,
  #     names_to = c("Time", ".value"),
  #     names_sep = "_"
  #   )
  
  DB_pre_post %>% 
    # Here we add the variables we want to pivot
    # The variable duplicates dont duplicate
    # If we want NA so we 
    select(ID_ECS, weights, 
           age_pre = age,sex_pre = sex,
           loneliness_pre,loneliness_post, 
           disability_pre, disability_post, 
           resilience_post) %>% 
    pivot_longer(
      # Here we add the variables we DONT want to pivot
      # The variable duplicates 
      cols = !c(ID_ECS, weights), 
      names_to = c(".value", "Time"),
      names_sep = "_"
    )
    
    
# LABELS ---not at the moment
# no assign yet 
# Check if is not necessary
# try to assign labels inside the analysis
# CREATE DATABASE WITH LABELS BUT NO ASSIGN IT 
