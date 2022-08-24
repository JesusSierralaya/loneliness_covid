# ··············································································
# FILE NAME:   graphs.R
# DESCRIPTION: Graphs
# 
# AUTHOR:      Jesus (jesus.sierralaya@inv.uam.es)
# 
# DATE:        23/08/2022
# 
# ·············································································· 

## ---- INCLUDES: --------------------------------------------------------------

library(here)
library(tidyverse)
library(effects)
library(labelled)
library(lme4)
library(ggtext)
library(patchwork)


## ---- PRE-PROCESS: --------------------------------------------------------------

# # Load data
# load(
#   here("dat","db_longer.Rda")
# )

# Recode
db_longer <- db_longer |> 
  to_factor() |> 
  mutate(Assessment =  Assessment |> 
           recode(`Pre-confinement` = "Before",
                  `Post-confinement` = "During"),
         SOLO2 = SOLO2 |> to_factor(),
         material = material |> to_factor(),
         ECON5 = ECON5 |> to_factor()
  )

# Parameters

# Axis limits
lim_inf <- 3.1
lim_sup <- 6.5
# Legend 
leg_dist <- 0
# Theme
theme_set(theme_minimal())
# color group
group.colors <- c("#D13D39", 
                  "#4070AE", 
                  "#C39C11",
                  "#3F706D",
                  "#705A81")

## ---- Loneliness total ----------------------------------------------------

# Fit
Fit_total <- lmer(Loneliness ~ 0 + Assessment + (1 | ID_ECS), 
                  data = db_longer, 
                  weights = db_longer$wfinal_norm)

# Effects
effects_total <- effect("Assessment", Fit_total) |> as.data.frame() |> 
      mutate(Assessment =  Assessment |> relevel("Before"))

# plot 
plot_total <- effects_total |>
  ggplot(aes(x = Assessment, y = fit)) + 
  # This stay the same
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Lonelines Total") +   
  ylab("Loneliness") +
  theme(plot.title = 
          element_text(hjust = 0.5, face = "bold", 
                       vjust = -7))

## ---- Age grouped ----------------------------------------------------

# Fit 
Fit_mix_age <- lmer(Loneliness ~ 0 + q1011_age_cat * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_age_cat <- effect("q1011_age_cat:Assessment", Fit_mix_age) |> as.data.frame() |>
            mutate(Assessment =  Assessment |> relevel("Before"),
           q1011_age_cat = q1011_age_cat |> fct_relevel("18-34", "35-49", "50-64"))

# plot
plot_age <- effects_age_cat |> ggplot(aes(x = Assessment, y = fit, 
                   group = q1011_age_cat, color = q1011_age_cat)) + 
   # This stay the same
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  guides(colour = guide_legend(title.position = "top", 
                               title.hjust = .5, nrow = 2)) +
  labs(color = "**Age grouped**")  +
  scale_color_manual(values = group.colors)

## ---- Sex ----------------------------------------------------

# Fit 
Fit_mix_sex <- lmer(Loneliness ~ 0 + q1009_sex * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_sex <- effect("q1009_sex:Assessment", Fit_mix_sex) |> as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"), 
             q1009_sex =  q1009_sex |> relevel("Female"))

# plot
plot_sex <- effects_sex |> 
  ggplot(aes(x = Assessment, y = fit, group = q1009_sex, color = q1009_sex)) + 
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(title.position = "top", 
                               title.hjust = .5, nrow = 2)) +
  labs(color = "**Sex**")  +
  scale_color_manual(values = group.colors)

## ---- Marital status ----------------------------------------------------

# Fit 
Fit_mix_marital <- lmer(Loneliness ~ 0 + q1012_mar_stat_recat * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_marital <- 
  effect("q1012_mar_stat_recat:Assessment", Fit_mix_marital) |> 
  as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"),
             q1012_mar_stat_recat = q1012_mar_stat_recat |> 
               relevel("Never married"))

# plot
plot_marital <- effects_marital |> 
  ggplot(aes(x = Assessment, y = fit, 
             group = q1012_mar_stat_recat, 
             # color = str_wrap(q1012_mar_stat_recat, 20))) +
             color = q1012_mar_stat_recat)) +
    geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(
    title.position = "top", title.hjust = .5, nrow = 3)) +
  labs(color = "**Marital status**") +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors)

## ---- Education level ----------------------------------------------------

# Fit 
Fit_mix_edu <- lmer(Loneliness ~ 0 + q1016_highest_recat * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_ed <- 
  effect("q1016_highest_recat:Assessment", Fit_mix_edu) |> # Change Here [2]
  as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"))

# plot
plot_ed <- effects_ed |> 
  ggplot(aes(x = Assessment, y = fit, 
             group = q1016_highest_recat, 
             color = q1016_highest_recat)) + 
    geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(
    title.position = "top", title.hjust = .5, nrow = 4)) +
  labs(color = "**Education level**") +
  scale_color_manual(values = group.colors)

## ---- Economy post ----------------------------------------------------

# Fit 
Fit_mix_economy_post <- lmer(Loneliness ~ 0 + economy_post * Assessment + (1 | ID_ECS), 
             data = db_longer, 
             weights = db_longer$wfinal_norm)

# Effects
effects_econ_post <- # Change name effects_var
  effect("economy_post:Assessment", Fit_mix_economy_post) |> # Change Here [2]
  as.data.frame() |>
    mutate(Assessment =  Assessment |> recode_factor(`Pre-confinement` = "Pre",
                                              `Post-confinement` = "Post",
                                              .ordered = TRUE))

# plot
plot_econ_post <- effects_econ_post |> # Copy name effects
  ggplot(aes(x = Assessment, y = fit,
             group = economy_post, 
             color = economy_post)) +
  # This stay the same
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5), width = .2) +
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(title.position = "top", 
                               title.hjust = .5, ncol = 1)) +
  labs(color = "**Worsening<br>economy**") +
  scale_color_manual(values = group.colors)

## ---- Social relationships ----------------------------------------------------

# Fit 
Fit_mix_social <- lmer(Loneliness ~ 0 + SOLO3 * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_social <- 
  effect("SOLO3:Assessment", Fit_mix_social) |> 
  as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"))

# plot
plot_social <- effects_social |> 
  ggplot(aes(x = Assessment, y = fit, 
             group = SOLO3, color = SOLO3)) + 
    geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(title.position = "top", 
                               title.hjust = .5, ncol = 1)) +
  labs(color = "**Social relationship**") +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors)

## ---- Living alone ----------------------------------------------------

# Fit 
Fit_mix_liv_alone_post <- lmer(Loneliness ~ 0 + living_alone_post * Assessment + (1 | ID_ECS), 
             data = db_longer, 
             weights = db_longer$wfinal_norm)

# Effects
effects_liv_alone_post <- # Change name effects_var
  effect("living_alone_post:Assessment", Fit_mix_liv_alone_post) |> 
  as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"))

# plot
plot_liv_alone_post <- effects_liv_alone_post |> # Copy name effects
  ggplot(aes(x = Assessment, y = fit, 
             group = living_alone_post, color = living_alone_post)) + # Change Here [2]
  # This stay the same
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  guides(colour = guide_legend(title.position = "top", 
                               title.hjust = .5, nrow = 2)) +
  labs(color = "**Living alone<br>(During)**")+
  scale_color_manual(values = group.colors)  

## ---- Social support before ----------------------------------------------------

# plot
plot_soc_sup_pre <- db_longer |> 
  drop_na() |> 
  ggplot(aes(x = social_support_pre_original, 
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  scale_x_continuous(#position = "top",
                     breaks = seq(3, 13, 3))  +
  theme(axis.title.x = element_blank(),
        # axis.title.y = element_blank(), 
        strip.placement = "outside"
        ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Social support<br>(Before)") +  
  ylab("Loneliness") +
  theme(plot.title = 
          element_markdown(face = "bold",
                       hjust = 0.5,
                       # vjust = -7
                       ))

## ---- Social support during ----------------------------------------------------

# plot
plot_soc_sup_post <- db_longer |> 
  drop_na() |> 
  ggplot(aes(x = social_support_post_original, 
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  scale_x_continuous(#position = "top",
                     breaks = seq(3, 13, 3))  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        strip.placement = "outside"
        ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Social support<br>(During)") +   
  theme(plot.title = 
          element_markdown(face = "bold",
                       hjust = 0.5,
                       # vjust = -7
                       ))

## ---- Physical activity post, (cat) ----------------------------------------------------

# Fit 
Fit_mix_physical_post <- lmer(Loneliness ~ 0 + physical_post * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_physical_post <-
  effect("physical_post:Assessment", Fit_mix_physical_post) |>
  as.data.frame() |>
    mutate(Assessment =  Assessment |> recode_factor(
      `Pre-confinement` = "Pre",
      `Post-confinement` = "Post",
      .ordered = TRUE))

# plot
plot_phys_post <- effects_physical_post |>
  ggplot(aes(x = Assessment, y = fit,
             group = physical_post, color = physical_post)) +   geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  guides(colour = guide_legend(title.position = "top", 
                               title.hjust = .5, ncol = 1)) +
  labs(color = "**Physical activity<br>(During)**")+
  ylab("Loneliness") +
  scale_color_manual(values = group.colors)  

## ---- Depresssion pre \[cat\] ---------------------------------------------------

# Fit 
Fit_mix_depre_pre <- lmer(Loneliness ~ 0 + depression_12m_pre * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_depre_pre <- # Change name effects_var
  effect("depression_12m_pre:Assessment", Fit_mix_depre_pre) |> # Change Here [2]
  as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"))

# plot
plot_depre_pre <- 
  effects_depre_pre |> 
  ggplot(aes(x = Assessment, y = fit, 
             group = depression_12m_pre, color = depression_12m_pre)) + 
    geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(title.position = "top", title.hjust = .5, nrow = 2)) +
  labs(color = "**Depression<br>(Before)**")+
  scale_color_manual(values = group.colors) 

## ---- Depression during \[cat\] ----------------------------------------------------

# Fit 
Fit_mix_depre_post <- lmer(Loneliness ~ 0 + depression_30d_post * Assessment + (1 | ID_ECS), 
             data = db_longer,
             weights = db_longer$wfinal_norm)

# Effects
effects_depre_post <- # Change name effects_var
  effect("depression_30d_post:Assessment", Fit_mix_depre_post) |> # Change Here [2]
  as.data.frame() |>
      mutate(Assessment =  Assessment |> relevel("Before"))

# plot
plot_depre_post <- effects_depre_post |> # Copy name effects
  ggplot(aes(x = Assessment, y = fit, 
             group = depression_30d_post, 
             color = depression_30d_post)) + 
    geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(.5), width = .2) + 
  theme(legend.position = "top",
        legend.title = element_markdown(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.margin = margin(t = leg_dist)
        ) + 
  coord_cartesian(ylim = c(lim_inf,lim_sup)) +
  guides(colour = guide_legend(title.position = "top", title.hjust = .5, nrow = 2)) +
  labs(color = "**Depression<br>(During)**")+
  scale_color_manual(values = group.colors) 

## ---- Disability pre ----------------------------------------------------

# plot 
plot_disability_pre <- db_longer |> # [Change here]
  ggplot(aes(x = whodas12_pre, # [Change here]
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  scale_x_continuous(limits = c(0,100), 
                     breaks = c(10, 50, 100))  +
  theme(#plot.title = element_markdown(),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(), 
        strip.placement = "outside"
        ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Disability<br>(Before)") +
  ylab("Loneliness") +
  # labs(title = "Disability<br>(Before)") +
  theme(plot.title = 
          element_markdown(face = "bold",
                       hjust = 0.5
                       ))


## ---- Disability pre ----------------------------------------------------

# plot

plot_disability_post <- db_longer |> # [Change here]
  drop_na(whodas12_post) |> 
  ggplot(aes(x = whodas12_post, # [Change here]
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  scale_x_continuous(limits = c(0,100),         # [Change here]
                     breaks = c(10, 50, 100))  + # [Change here]
  theme(#plot.title = element_markdown(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    strip.placement = "outside"
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Disability<br>(During)") +
  theme(plot.title = 
          element_markdown(face = "bold",
                           hjust = 0.5
          ))

## ---- Neuroticism ----------------------------------------------------

# plot
plot_neuroticism <- db_longer |> # [Change here]
  # drop_na(whodas12_post) |> 
  ggplot(aes(x = neuroticism, # [Change here]
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  # scale_x_continuous(limits = c(0,100),         # [Change here]
  #                    breaks = c(10, 50, 100))  + # [Change here]
  theme(#plot.title = element_markdown(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    strip.placement = "outside"
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Neuroticism") +
  theme(plot.title = 
          element_markdown(face = "bold",
                           hjust = 0.5
          ))


## ---- Extraversion ----------------------------------------------------

# plot
plot_extraversion <- db_longer |> # [Change here]
  # drop_na(whodas12_post) |> 
  ggplot(aes(x = extraversion, # [Change here]
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  # scale_x_continuous(limits = c(0,100),         # [Change here]
  #                    breaks = c(10, 50, 100))  + # [Change here]
  theme(#plot.title = element_markdown(),
    axis.title.x = element_blank(),
    # axis.title.y = element_blank(), 
    strip.placement = "outside"
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Extraversion") +
  ylab("Loneliness") +
  theme(plot.title = 
          element_markdown(face = "bold",
                           hjust = 0.5
          ))

## ---- Resilience ----------------------------------------------------

# plot
plot_resilience <- db_longer |> # [Change here]
  # drop_na(whodas12_post) |> 
  ggplot(aes(x = resilience_scale_post, # [Change here]
             y = Loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Assessment, switch = "both") +
  # scale_x_continuous(limits = c(0,100),         # [Change here]
  #                    breaks = c(10, 50, 100))  + # [Change here]
  theme(#plot.title = element_markdown(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    strip.placement = "outside"
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Resilience<br>(During)") +
  # ylab("Loneliness") +
  theme(plot.title = 
          element_markdown(face = "bold",
                           hjust = 0.5
          ))

## ---- Merge Sociodemographics ----------------------------------------------------

# Remove axis ticks from second and third subplot
plot_age <- plot_age + 
  theme(axis.text.y = element_blank())
plot_sex <- plot_sex +   
  scale_y_continuous(position = "right")
plot_ed <- plot_ed + 
  theme(axis.text.y = element_blank())
plot_econ_post <- plot_econ_post +   
  scale_y_continuous(position = "right")

# Add spacing between row subplots
plot_total <- plot_total + 
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_age <- plot_age +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_sex <- plot_sex +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))

# Text size 
text_size <- 19

plot_total <- plot_total + 
  theme(text = element_text(size = text_size)) 

plot_age <- plot_age +
  theme(text = element_text(size = text_size)) 

plot_sex <- plot_sex +
  theme(text = element_text(size = text_size)) 

plot_marital <- plot_marital + 
  theme(text = element_text(size = text_size))  

plot_ed <- plot_ed + 
  theme(text = element_text(size = text_size)) 

plot_econ_post <- plot_econ_post + 
  theme(text = element_text(size = text_size)) 

# plot
plot_sociod <- (plot_total + plot_age + plot_sex) / 
  (plot_marital + plot_ed + plot_econ_post)

## ---- Merge social aspects ---------------------------------------------------

# Remove axis ticks from second and third subplot
plot_liv_alone_post <- plot_liv_alone_post + 
  scale_y_continuous(position = "right")
plot_soc_sup_post <- plot_soc_sup_post +
  scale_y_continuous(position = "right")

# Add spacing between row subplots
plot_social <- plot_social + 
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_liv_alone_post <- plot_liv_alone_post +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))

# Text size 
text_size <- 19

plot_social <- plot_social + 
  theme(text = element_text(size = text_size)) 

plot_liv_alone_post <- plot_liv_alone_post +
  theme(text = element_text(size = text_size)) 

plot_soc_sup_pre <- plot_soc_sup_pre +
  theme(text = element_text(size = text_size)) 

plot_soc_sup_post <- plot_soc_sup_post +
  theme(text = element_text(size = text_size))

# Plot
plot_social <- (plot_social + plot_liv_alone_post) / 
  (plot_soc_sup_pre + plot_soc_sup_post)

## ---- Merge Health and wellbeing ---------------------------------------------

# Text axis without or to the right
plot_depre_pre <- plot_depre_pre + 
  theme(axis.text.y = element_blank())
plot_depre_post <- plot_depre_post + 
  scale_y_continuous(position = "right")

plot_disability_post <- plot_disability_post + 
  theme(axis.text.y = element_blank())
plot_neuroticism <- plot_neuroticism + 
  scale_y_continuous(position = "right")

plot_resilience <- plot_resilience + 
  theme(axis.text.y = element_blank())

# Add spacing between row subplots
plot_phys_post <- plot_phys_post + 
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_depre_pre <- plot_depre_pre +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_depre_post <- plot_depre_post +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_disability_pre <- plot_disability_pre + 
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_disability_post <- plot_disability_post +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))
plot_neuroticism <- plot_neuroticism +
  theme(plot.margin = unit(c(0,0,50,0), "pt"))

# Text size 
text_size <- 19

plot_phys_post <- plot_phys_post + 
  theme(text = element_text(size = text_size)) 

plot_depre_pre <- plot_depre_pre + 
  theme(text = element_text(size = text_size)) 

plot_depre_post <- plot_depre_post + 
  theme(text = element_text(size = text_size)) 

plot_disability_pre <- plot_disability_pre + 
  theme(text = element_text(size = text_size)) 

plot_disability_post <- plot_disability_post + 
  theme(text = element_text(size = text_size)) 

plot_neuroticism <- plot_neuroticism + 
  theme(text = element_text(size = text_size)) 

plot_extraversion <- plot_extraversion + 
  theme(text = element_text(size = text_size)) 

plot_resilience <- plot_resilience + 
  theme(text = element_text(size = text_size)) 

# plot
plot_health <- 
  (plot_phys_post + plot_depre_pre + plot_depre_post) /
  (plot_disability_pre + plot_disability_post + plot_neuroticism) /
  (plot_extraversion + plot_resilience + plot_spacer()) 
