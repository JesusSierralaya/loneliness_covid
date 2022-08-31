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

library(tidyverse)
library(effects)
# library(labelled)
library(lme4)
library(ggtext)
library(patchwork)


## ---- PRE-PROCESS: -----------------------------------------------------------

  # # Re-code
  # DB_graphs <- DB_graphs |> 
  #   to_factor() |> 
  #   mutate(Time =  Time |> 
  #            recode(`Pre-confinement` = "Before",
  #                   `Post-confinement` = "During"),
  #          SOLO2 = SOLO2 |> to_factor(),
  #          material = material |> to_factor(),
  #          ECON5 = ECON5 |> to_factor()
  #   )
  
  # Axis limits
  lim_inf <- 3.1
  lim_sup <- 6.5
  
  # Legend 
  leg_dist <- 0
  
  # Theme
  theme_set(theme_minimal())
  
  # color groups
  group.colors <- c("#D13D39", 
                    "#4070AE", 
                    "#C39C11",
                    "#3F706D",
                    "#705A81")

## ---- loneliness total ----------------------------------------------------

  # Fit
  Fit_total <- lmer(loneliness ~ 0 + Time + (1 | ID_ECS), 
                      data = DB_graphs, 
                      weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_total <- effect("Time", Fit_total) |> as.data.frame()
  
  # plot 
  plot_total <- effects_total |>
    ggplot(aes(x = Time, y = fit)) + 
    # This stay the same
    geom_point(position = position_dodge(.5)) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(.5), width = .2) + 
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) +
    ggtitle("Lonelines Total") +   
    ylab("Loneliness") +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = "bold", 
                                    vjust = -7))
## ---- Age grouped ----------------------------------------------------

  # Fit
  Fit_age <- lmer(loneliness ~ 0 + age * Time + (1 | ID_ECS),
                      data = DB_graphs,
                      weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_age_cat <- effect("age:Time", Fit_age) |>
    as.data.frame() #|>
  #   mutate(Time =  Time |> relevel("Before"),
  #          q1011_age_cat = q1011_age_cat |> fct_relevel("18-34", "35-49", "50-64"))
  
  # plot
  plot_age <- effects_age_cat %>% 
    ggplot(aes(x = Time, y = fit,
               group = age, color = age)) +
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
  Fit_sex <- lmer(loneliness ~ 0 + sex * Time + (1 | ID_ECS), 
                  data = DB_graphs,
                  weights = DB_graphs %>% pull(weights))

  # Effects
  effects_sex <- effect("sex:Time", Fit_sex) |> as.data.frame() 
    
  # plot
  plot_sex <- effects_sex |> 
    ggplot(aes(x = Time, y = fit, group = sex, color = sex)) + 
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

## ---- Education level ----------------------------------------------------
  
  # Fit 
  Fit_educlevel <- lmer(loneliness ~ 0 + educlevel * Time + (1 | ID_ECS), 
                        data = DB_graphs,
                        weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_educlevel <- 
    effect("educlevel:Time", Fit_educlevel) %>% as.data.frame()
  
  # plot
  plot_ed <- effects_educlevel |> 
    ggplot(aes(x = Time, y = fit, 
               group = educlevel, 
               color = educlevel)) + 
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

## ---- Marital status ----------------------------------------------------

  # Fit 
  Fit_marital <- lmer(loneliness ~ 0 + maritalstatus * Time + (1 | ID_ECS), 
                          data = DB_graphs,
                          weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_marital <- 
    effect("maritalstatus:Time", Fit_marital) |> 
    as.data.frame() 
  
  # plot
  plot_marital <- effects_marital |> 
    ggplot(aes(x = Time, y = fit, 
               group = maritalstatus, 
               color = maritalstatus)) +
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

## ---- VIRTUAL CONTACT POST --------------------------------------------------
  
  # Fit 
  Fit_virtual <- lmer(loneliness ~ 0 + virtualcontact_post * Time + (1 | ID_ECS), 
                      data = DB_graphs,
                      weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_virtual <- 
    effect("virtualcontact_post:Time", Fit_virtual)  %>% 
    as.data.frame() 
  
  # plot
  plot_virtual <- effects_virtual |> 
    ggplot(aes(x = Time, y = fit, 
               group = virtualcontact_post, 
               color = virtualcontact_post)) +
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
      title.position = "top", title.hjust = .5, nrow = 4)) +
    labs(color = "**Virtual Contact (During)**") +
    ylab("Loneliness") +
    scale_color_manual(values = group.colors)

## ---- SOCIAL CHANGES POST (SOLO3) --------------------------------------
  
  # Fit 
  Fit_social_changes <- lmer(loneliness ~ 0 + socialchanges_post * Time + 
                               (1 | ID_ECS), 
                         data = DB_graphs,
                         weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_social_changes <- 
    effect("socialchanges_post:Time", Fit_social_changes) %>%  
    as.data.frame()
  
  # plot
  plot_social_changes <- effects_social_changes  %>%  
    ggplot(aes(x = Time, y = fit, 
               group = socialchanges_post, 
               color = socialchanges_post)) + 
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
    labs(color = "**Social Changes (During)**") +
    ylab("Loneliness") +
    scale_color_manual(values = group.colors)
  
  
## ---- ECONOMY WORSENED POST ----------------------------------------------

  # Fit 
  Fit_economy_post <- lmer(loneliness ~ 0 + economyworsened_post * Time + 
                             (1 | ID_ECS), 
                               data = DB_graphs, 
                               weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_economy_post <- # Change name effects_var
    effect("economyworsened_post:Time", Fit_economy_post) |> # Change Here [2]
    as.data.frame() 
  
  # plot
  plot_econ_post <- effects_economy_post |> # Copy name effects
    ggplot(aes(x = Time, y = fit,
               group = economyworsened_post, 
               color = economyworsened_post)) +
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
    labs(color = "**Economy<br>Worsening (During)**") +
    scale_color_manual(values = group.colors)

## ---- UNEMPLOYMENT POST ----------------------------------------------
  
  # Fit 
  Fit_unemployment_post <- lmer(loneliness ~ 0 + unemployment_post * Time + 
                             (1 | ID_ECS), 
                           data = DB_graphs, 
                           weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_unemployment_post <- # Change name effects_var
    effect("unemployment_post:Time", Fit_unemployment_post) |> 
    as.data.frame() 
  
  # plot
  plot_unemployment_post <- effects_unemployment_post |> # Copy name effects
    ggplot(aes(x = Time, y = fit,
               group = unemployment_post, 
               color = unemployment_post)) +
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
    labs(color = "**Unemployment (During)**") +
    scale_color_manual(values = group.colors)

## ---- MATERIAL DEPRIVATION --------------------------------------------------
  
  # Fit 
  Fit_material_pre <- lmer(loneliness ~ 0 + materialdeprivation_pre * Time + 
                                  (1 | ID_ECS), 
                                data = DB_graphs, 
                                weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_material_pre<- # Change name effects_var
    effect("materialdeprivation_pre:Time", Fit_material_pre) |> 
    as.data.frame() 
  
  # plot
  plot_material_pre <- effects_material_pre |> # Copy name effects
    ggplot(aes(x = Time, y = fit,
               group = materialdeprivation_pre, 
               color = materialdeprivation_pre)) +
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
    labs(color = "**Material<br>Deprivation  (Before)**") +
    scale_color_manual(values = group.colors)
  
## ---- LIVING ALONE POST ----------------------------------------------------

# Fit 
Fit_mix_liv_alone_post <- lmer(loneliness ~ 0 + living_alone_post * Time + (1 | ID_ECS), 
                               data = DB_graphs, 
                               weights = DB_graphs %>% pull(weights))

# Effects
effects_liv_alone_post <- # Change name effects_var
  effect("living_alone_post:Time", Fit_mix_liv_alone_post) |> 
  as.data.frame() |>
  mutate(Time =  Time |> relevel("Before"))

# plot
plot_liv_alone_post <- effects_liv_alone_post |> # Copy name effects
  ggplot(aes(x = Time, y = fit, 
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
plot_soc_sup_pre <- DB_graphs |> 
  drop_na() |> 
  ggplot(aes(x = social_support_pre_original, 
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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
plot_soc_sup_post <- DB_graphs |> 
  drop_na() |> 
  ggplot(aes(x = social_support_post_original, 
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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
Fit_mix_physical_post <- lmer(loneliness ~ 0 + physical_post * Time + (1 | ID_ECS), 
                              data = DB_graphs,
                              weights = DB_graphs %>% pull(weights))

# Effects
effects_physical_post <-
  effect("physical_post:Time", Fit_mix_physical_post) |>
  as.data.frame() |>
  mutate(Time =  Time |> recode_factor(
    `Pre-confinement` = "Pre",
    `Post-confinement` = "Post",
    .ordered = TRUE))

# plot
plot_phys_post <- effects_physical_post |>
  ggplot(aes(x = Time, y = fit,
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
Fit_mix_depre_pre <- lmer(loneliness ~ 0 + depression_12m_pre * Time + (1 | ID_ECS), 
                          data = DB_graphs,
                          weights = DB_graphs %>% pull(weights))

# Effects
effects_depre_pre <- # Change name effects_var
  effect("depression_12m_pre:Time", Fit_mix_depre_pre) |> # Change Here [2]
  as.data.frame() |>
  mutate(Time =  Time |> relevel("Before"))

# plot
plot_depre_pre <- 
  effects_depre_pre |> 
  ggplot(aes(x = Time, y = fit, 
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
Fit_mix_depre_post <- lmer(loneliness ~ 0 + depression_30d_post * Time + (1 | ID_ECS), 
                           data = DB_graphs,
                           weights = DB_graphs %>% pull(weights))

# Effects
effects_depre_post <- # Change name effects_var
  effect("depression_30d_post:Time", Fit_mix_depre_post) |> # Change Here [2]
  as.data.frame() |>
  mutate(Time =  Time |> relevel("Before"))

# plot
plot_depre_post <- effects_depre_post |> # Copy name effects
  ggplot(aes(x = Time, y = fit, 
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
plot_disability_pre <- DB_graphs |> # [Change here]
  ggplot(aes(x = whodas12_pre, # [Change here]
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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

plot_disability_post <- DB_graphs |> # [Change here]
  drop_na(whodas12_post) |> 
  ggplot(aes(x = whodas12_post, # [Change here]
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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
plot_neuroticism <- DB_graphs |> # [Change here]
  # drop_na(whodas12_post) |> 
  ggplot(aes(x = neuroticism, # [Change here]
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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
plot_extraversion <- DB_graphs |> # [Change here]
  # drop_na(whodas12_post) |> 
  ggplot(aes(x = extraversion, # [Change here]
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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
plot_resilience <- DB_graphs |> # [Change here]
  # drop_na(whodas12_post) |> 
  ggplot(aes(x = resilience_scale_post, # [Change here]
             y = loneliness)) + 
  geom_smooth(method = "lm") +
  facet_grid(~Time, switch = "both") +
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
