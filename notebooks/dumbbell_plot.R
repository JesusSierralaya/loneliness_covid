
# here im going to try to draw a dumbbell plot
# im think this is the better way to represent paired data

# include
library(tidyverse)
library(here)
library(paint)
library(patchwork)
# 

# Database
source(here("throughput", "import_clean.R"))

# First im going to try this method, I think is more controlled

# https://youtu.be/qaksmQabMUI
# https://rpubs.com/mathetal/dumbbell

# Format of dataframe
# we need a specific form of the data to represent in the dumbbell

# with summary means

# VARIABLES     |  TIME  |  LONELINESS  |
# Male          |  Pre   |      3       |
# Male          |  Post  |      4       |
# Female        |  Pre   |      5       |
# Female        |  Post  |      6       |

# Resilience_pre|  Pre   |      5       |
# Resilience_pre|  Post  |      6       |

# Soc_supp_pre  |  Pre   |      7       |
# Soc_supp_pre  |  Post  |      8       |
# Soc_supp_post |  Pre   |      9       |
# Soc_supp_pre  |  Post  |      7       |

# DB_pre_post %>% 
#   group_by(sex) %>% 
#   summarise(mean = mean(loneliness_post))

summ_sex <- DB_graphs %>% 
   group_by(sex, Time) %>% 
   summarise(mean = mean(loneliness))

summ_dep <- DB_graphs %>% 
  group_by(depression_pre, Time) %>% 
  summarise(mean = mean(loneliness))


list_summ <- list(summ_sex, summ_dep)

a <- summ_sex %>% 
  ggplot(aes(mean, sex, color = Time)) + 
  geom_line(color = "black") +
  geom_point(size = 10, alpha = 0.5) +
  xlim(3, 9) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position="top")

b <- summ_dep %>% 
  ggplot(aes(mean, depression_pre , color = Time)) + 
  geom_line(color = "black") +
  geom_point(size = 10, alpha = 0.5) +
  xlim(3, 9) +
  theme(legend.position="none")

a/b

# https://riffomonas.org/code_club/2021-08-12-aug-oct-ipsos



