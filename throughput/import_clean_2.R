# ··············································································
# FILE NAME:   import_clean.R
# DESCRIPTION: Import, clean and transform the database
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        17/08/2022
# 
# ·············································································· 
# NOTES 
# - db_* databases that will collapse have the prefix db_*
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

# PARAMETERS -------------------------------------------------------------------

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
    read_dta(col_select = all_of(c("ID_ECS", # Id 
                                   "q1009_sex")))
  
  ### TRANSFORMATION ---
  
  db_sex %<>% rename(sex = q1009_sex) # Change name

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


# MERGE ------------------------------------------------------------------------
  
  # Remove all no db_ 
  rm(list=setdiff(ls(), ls(pattern = "db_")))
  
  # Merge
  # full_join(list(db_age, db_filter, db_loneliness), by = "ID_ECS")
  # list(db_age, db_filter, db_loneliness) %>% reduce(full_join, by = "ID_ECS")
  
  DB_pre_post_full <- lapply(ls(pattern="db_"), get) %>% reduce(full_join, by = "ID_ECS")
  
  rm(list=setdiff(ls(), ls(pattern = "DB_")))
  # Test with: map_dfr() 

# CLEAN DATA -------------------------------------------------------------------

## REVIEW ----------------------------------------------------------------------

# TRANSFORMATION --------------------------------------------------------------

## LONELINESS PRE-POST ------------

## SEX ----------------------------

## REVIEW

# CREATE DATA BASES --------------------------------------

# LABELS ---not at the moment
# no assign yet 
# Check if is not necessary
# try to assign labels inside the analysis
# CREATE DATABASE WITH LABELS BUT NO ASSIGN IT 
