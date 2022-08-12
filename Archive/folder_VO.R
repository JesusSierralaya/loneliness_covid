# VO

# Physical health / DISABILITY####

# Carpeta pre y post
DB_FILE_DISAB_PRE <- file.path(CB_FILE_OUTCOMES_PRE, "Outcome_disability.dta")
DB_FILE_DISAB_POST <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_disability.dta")
# Carga datos
disability_pre <- DB_FILE_DISAB_PRE |> read_dta()
disability_post <- DB_FILE_DISAB_POST |> read_dta()
# Merge 
DB_VO <- DB_filtered |> 
  left_join(disability_pre, by = "ID_ECS") |> 
  select(whodas12) 
DB_filtered$whodas12_pre <- DB_VO$whodas12
DB_VO <- DB_filtered |> 
  left_join(disability_post, by = "ID_ECS") |> 
  select(whodas12)
DB_filtered$whodas12_post <- DB_VO$whodas12

# Social support ####

# Carpeta pre y post
DB_FILE_SOCSUPP_PRE <- file.path(CB_FILE_OUTCOMES_PRE, "Outcome_Loneliness, social support and social isolation.dta")
DB_FILE_SOCSUPP_POST <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_loneliness and social support_Subestudio_covid.dta")
# Carpeta pre y post
socsupp_pre <- DB_FILE_SOCSUPP_PRE |> read_dta()
socsupp_post <- DB_FILE_SOCSUPP_POST |> read_dta()
# Merge
DB_VO <- DB_filtered |> 
  left_join(socsupp_pre, by = "ID_ECS") |> 
  select(social_support) 
DB_filtered$social_support_pre <- DB_VO$social_support

DB_VO_post <- DB_filtered |> 
  left_join(socsupp_post, by = "ID_ECS") |> 
  select(social_support)
DB_filtered$social_support_post <- DB_VO_post$social_support

# Living alone ####

# Carpeta post
DB_FILE_LIVEALONE_POST <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_social interactions_Subestudio_covid.dta")
# Carga datos
livealone_post <- DB_FILE_LIVEALONE_POST |> read_dta()
# Merge
DB_VO <- DB_filtered |>
  left_join(livealone_post, by = "ID_ECS") |>
  select(living_alone)
DB_filtered$living_alone <- DB_VO$living_alone

# Personalidad ####
# Carpeta pre
DB_FILE_PERSON <- file.path(CB_FILE_OUTCOMES_PRE, "Outcome_EPQR-A.dta")
# # Carga datos
personalidad <- DB_FILE_PERSON |> read_dta()
# # Merge
DB_VO <- DB_filtered |>
  left_join(personalidad, by = "ID_ECS") |>
  select(neuroticism, extraversion, sincerity)
DB_filtered$neuroticism <- DB_VO$neuroticism
DB_filtered$extraversion <- DB_VO$extraversion
DB_filtered$sincerity <- DB_VO$sincerity

# Physical ####
# Carpeta 
DB_FILE_PHYSICAL <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_physical_activity.dta")
# # Carga datos
physical <- DB_FILE_PHYSICAL |> read_dta()
# Merge
DB_VO <- DB_filtered |>
  left_join(physical, by = "ID_ECS") |>
  select(physical)
DB_filtered$physical <- DB_VO$physical

# Resiliencia ####
# Carpeta 
DB_FILE_RESILIENCE <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_brief resilience scale_Subestudio_covid.dta")
# # Carga datos
resilience <- DB_FILE_RESILIENCE |> read_dta()
# Merge
DB_VO <- DB_filtered |>
  left_join(resilience, by = "ID_ECS") |>
  select(resilience_scale)
DB_filtered$resilience_scale <- DB_VO$resilience_scale

# Depression pre ####
DB_FILE_DEPRE <- file.path(CB_FILE_OUTCOMES_PRE, "Outcome_depression_ICD10.dta")
# # Carga datos
depression_pre <- DB_FILE_DEPRE |> read_dta()
# Merge
DB_VO <- DB_filtered |>
  left_join(depression_pre, by = "ID_ECS") |>
  select(depression_12m)
DB_filtered$depression_12m_pre <- DB_VO$depression_12m

# Depression post ####
DB_FILE_DEPRE_POST <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_depression_ICD10.dta")
depression_post <- DB_FILE_DEPRE_POST |> read_dta()
# Merge
DB_VO <- DB_filtered |>
  left_join(depression_post, by = "ID_ECS") |>
  select(depression_30d)
DB_filtered$depression_30d_post <- DB_VO$depression_30d

# Anxiety ola 1 ####
DB_FILE_ANXIETY_PRE <- file.path(CB_FILE_OUTCOMES_PRE, "Outcome_anxietyDSM4.dta")
anxiety_pre <- DB_FILE_ANXIETY_PRE |> read_dta()
# merge
DB_VO <- DB_filtered |>
  left_join(anxiety_pre, by = "ID_ECS") |>
  select(GAD12m)
DB_filtered$GAD12m_pre <- DB_VO$GAD12m

# Panic ola 1 ####
DB_FILE_PANIC_PRE <- file.path(CB_FILE_OUTCOMES_PRE, "Outcome_panic.dta")
panic_pre <- DB_FILE_PANIC_PRE |> read_dta()
# merge
DB_VO <- DB_filtered |>
  left_join(panic_pre, by = "ID_ECS") |>
  select(PAN12m)
DB_filtered$PAN12m_pre <- DB_VO$PAN12m


# Economic post ###
DB_FILE_ECON_POST <- file.path(CB_FILE_OUTCOMES_POST, "Outcome_economic.dta")
econ_post <- DB_FILE_ECON_POST |> read_dta()
# Merge
DB_VO <- DB_filtered |>
  left_join(econ_post, by = "ID_ECS") |>
  select(economy)
DB_filtered$economy_post <- DB_VO$economy
