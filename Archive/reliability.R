# Reliability

# Loneliness

LONELINESS_PRE_ITEMS  <- c("q6351_companion", "q6352_leftout", "q6353_isolated")
LONELINESS_POST_ITEMS <- c("SOLO7_1", "SOLO7_2", "SOLO7_3")

alpha_loneliness_pre <- DB_2 %>%
  select(all_of(LONELINESS_PRE_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

alpha_loneliness_post <- DB_2 %>%
  select(all_of(LONELINESS_POST_ITEMS)) %>%
  psych::alpha(check.keys = TRUE) %>%
  extract2(c("total", "raw_alpha"))

# # Physical health ola 1

