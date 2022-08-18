# ··············································································
# FILE NAME:   reliability.R
# DESCRIPTION: Compute the reability
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        17/08/2022
# 
# ·············································································· 

## ---- INCLUDES: --------------------------------------------------------------


library(haven)
library(readstata13)
library(tidyverse)
library(magrittr)
library(gtsummary)

## ---- PATHS: -----------------------------------------------------------------

# File system:
# BASE_DIR <- "~/../UAM"
BASE_DIR <- "~/../../UAM"
DB_PATH_MAIN <- file.path(
  BASE_DIR,
  "marta.miret@uam.es - Bases de datos maestras Edad con Salud"
)
DB_PRE_DIR <- file.path(
  DB_PATH_MAIN,
  "Ola_3/Cohorte_2019"
)
DB_POST_DIR <- file.path(DB_PATH_MAIN, "Subestudio_COVID")

# Source datasets:
DB_PRE_PATH <- file.path(
  DB_PRE_DIR,
  "rawdata_c2019w1.dta"
)
DB_POST_PATH <- file.path(
  DB_POST_DIR,
  "Edad_con_salud_Fichero_Completo.dta"
)

# Load data/ Preprocess data

#~[borrar]

# Variable definitions

# pre
FILTERS_PRE_ITEMS <- c("ID_ECS" , "subsample_pre", "q6010h_neighbours",
                       "q0007a_result")

LONELINESS_PRE_ITEMS <- c("q6351_companion", "q6352_leftout", "q6353_isolated")

DISABILITY_PRE_ITEMS <- c(
  "q2028_stand",
  "q2032_hh_resp",
  "q2011_learn",
  "q2033_activ",
  "q2047_affect",
  "q2035_concent",
  "q2036_walk",
  "q2037_wash",
  "q2038_dress",
  "q2015_strang",
  "q2014_friend",
  "q2039_daily"
)

SOCIAL_SUPPORT_PRE_ITEMS <- c(
  "q6310_help_neig", "q6320_close", "q6330_concern"
)

NEUROTICISM_PRE_ITEMS <- c(
  "q4641_mood", 
  "q4646_fedup", 
  "q4648_nervous", 
  "q4650_worried", 
  "q4653_nerves", 
  "q4656_alone")

EXTRAVERSION_PRE_ITEMS <- c(
  "q4642_talkative", 
  "q4643_lively", 
  "q4649_party", 
  "q4651_meetings", 
  "q4655_quiet", 
  "q4657_livelyothers"
)

# post

FILTERS_POST_ITEMS <- c("ID_ECS" , "ESTADO_ENTREVISTA")

LONELINESS_POST_ITEMS <- c("SOLO7_1", "SOLO7_2", "SOLO7_3")

DISABILITY_POST_ITEMS <- "SF2_" %>% paste0(1:12)

PHYSICAL_POST_ITEMS <- c(
  "AF1A", "AF1B_H_1", "AF1B_M_1",
  "AF2A", "AF2B_H_1", "AF2B_M_1",
  "AF3A", "AF3B_H_1", "AF3B_M_1"
)

RESILIENCE_POST_ITEMS <- c(
  "SM26_1", "SM26_2", "SM26_3", "SM26_4", "SM26_5", "SM26_6"
)

SOCIAL_SUPPORT_POST_ITEMS <- c("SOLO9A", "SOLO9B", "SOLO9C")

# Missing
ITEMS_WITH_MISSING_PRE  <- c(SOCIAL_SUPPORT_PRE_ITEMS,  DISABILITY_PRE_ITEMS)
ITEMS_WITH_MISSING_POST <- c(SOCIAL_SUPPORT_POST_ITEMS, DISABILITY_POST_ITEMS)

### extra - test
DISABILITY_RECODE_PRE  <- c(
  "q2032_hh_resp",
  "q2033_activ",
  "q2037_wash",
  "q2015_strang",
  "q2014_friend",
  "q2039_daily"
)
DISABILITY_RECODE_POST <- "SF2_" %>% paste0(c(2, 4, 8, 10, 11, 12))


## ---- LOAD: -----------------------------------------------------------------

PRE_ITEMS <- unlist(mget(ls(pattern = "PRE_ITEMS")), use.names = FALSE)

POST_ITEMS <- unlist(mget(ls(pattern = "POST_ITEMS")), use.names = FALSE)


# pre
db_pre <- DB_PRE_PATH |> 
  read_dta(col_select = all_of(PRE_ITEMS)) #|> 
  # filter(subsample_pre == 1) 

# post
db_post <- DB_POST_PATH |> 
  read.dta13(select.cols = POST_ITEMS) # |>
  # filter(ESTADO_ENTREVISTA == 1) |> 
  # tibble::tibble()

db_1 <- full_join(db_pre, db_post, by = "ID_ECS") |>
  filter(subsample_pre == 1 & ESTADO_ENTREVISTA == 1)

# pre process
db_1 <- db_1 %>% mutate(
  # pre
  # Missing values(disability)
  across(all_of(ITEMS_WITH_MISSING_PRE), na_if, 888),
  across(all_of(ITEMS_WITH_MISSING_PRE), na_if, 999),
  # Values missing by design in social support item:
  across(q6310_help_neig, ~if_else(q6010h_neighbours == 2, 5, as.numeric(.))),
  # Recode response categories in disability:
  across(all_of(DISABILITY_RECODE_PRE), as.numeric),
  across(all_of(DISABILITY_RECODE_PRE), recode, `2` = 3, `4` = 5),
  
  # post
  # Missing values (disability)
  across(all_of(ITEMS_WITH_MISSING_POST), na_if, 8),
  across(all_of(ITEMS_WITH_MISSING_POST), na_if, 9),
  # Recode response categories in disability:
  across(all_of(DISABILITY_RECODE_POST), as.numeric),
  across(all_of(DISABILITY_RECODE_POST), recode, `2` = 3, `4` = 5),
  # Recode physical activity items:
  across(all_of(PHYSICAL_POST_ITEMS), as.numeric),
  across(
    all_of(PHYSICAL_POST_ITEMS),
    ~if_else(ESTADO_ENTREVISTA ==  1, recode(., `99` = 0), .)
  ),
  across(
    all_of(PHYSICAL_POST_ITEMS) & matches("B_H_1$"), `*`, 60
  ),
  met1 = AF1A * (AF1B_H_1 + AF1B_M_1) * 8,
  met2 = AF2A * (AF2B_H_1 + AF2B_M_1) * 4,
  met3 = AF3A * (AF2B_H_1 + AF3B_M_1) * 4
 )
# Preprocess data

# Missing values

## ---- REABILITY: -------------------------------------------------------------

# pre

alpha_loneliness_pre <- db_1 |> 
  select(all_of(LONELINESS_PRE_ITEMS))  |> 
  psych::alpha(check.keys = TRUE)  |> 
  extract2(c("total", "raw_alpha"))

alpha_disability_pre <- db_1 %>%
  select(all_of(DISABILITY_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

alpha_social_support_pre <- db_1 %>%
  select(all_of(SOCIAL_SUPPORT_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

alpha_neuroticism_pre <- db_1 %>%
  select(all_of(NEUROTICISM_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

alpha_extraversion_pre <- db_1 %>%
  select(all_of(EXTRAVERSION_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

# post

alpha_loneliness_post <- db_1 %>%
  select(all_of(LONELINESS_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

alpha_disability_post <- db_1 %>%
  select(all_of(DISABILITY_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))
# 
# alpha_physical_post <- db_1 %>%
#   select(all_of(PHYSICAL_POST_ITEMS)) %>%
#   psych::alpha(check.keys = TRUE) %>%
#   extract2(c("total", "raw_alpha"))

alpha_resilience_post <- db_1 %>%
  select(all_of(RESILIENCE_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))
# 
alpha_social_support_post <- db_1 %>%
  select(all_of(SOCIAL_SUPPORT_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

# Generación de tabla
# mget(ls(pattern = "alpha_"))
Variable <- ls(pattern = "alpha_")
Alpha <- unlist(mget(Variable), use.names = FALSE)
# c.1 <- unlist(mget(ls(pattern = "alpha_")))
reliability <- cbind(Variable,Alpha)
reliability[,1] <- str_replace(reliability[,1], "alpha_","")
reliability <- as.data.frame(reliability)
order <- c(
  "loneliness_pre",
  "loneliness_post",
  "disability_pre",
  "disability_post",
  "social_support_pre",
  "social_support_post",
  "resilience_post",
  "neuroticism_pre",
  "extraversion_pre"
)
reliability <- reliability |> slice(match(order,Variable))
reliability$Alpha <- reliability |> pull(Alpha) |> as.numeric() |> round(2)

## ---- SAVE Rda: --------------------------------------------------------------

save(reliability, file = "dat/reliability.Rda")
