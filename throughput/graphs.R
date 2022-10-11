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
library(magrittr)

## ---- PRE-PROCESS: -----------------------------------------------------------

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

## ---- LONELINESS TOTAL ----------------------------------------------------

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
    # ggtitle("Lonelines Total") +   
    ylab("Loneliness") #+
    # theme(plot.title = element_text(hjust = 0.5, 
    #                                 face = "bold", 
    #                                 vjust = -7))
## ---- AGE ----------------------------------------------------

  # Fit
  Fit_age <- lmer(loneliness ~ 0 + age * Time + (1 | ID_ECS),
                      data = DB_graphs,
                      weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_age_cat <- effect("age:Time", Fit_age) |>
    as.data.frame() #|>

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
          # axis.title.y = element_blank(),
          legend.margin = margin(t = leg_dist)
    ) +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) +
    guides(colour = guide_legend(title.position = "top",
                                 title.hjust = .5, nrow = 2)) +
    labs(color = "**Age grouped**")  +
    ylab("Loneliness") +
    scale_color_manual(values = group.colors)

## ---- SEX ----------------------------------------------------

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

## ---- EDUCACION LEVEL ----------------------------------------------------
  
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

## ---- MARITAL STATUS ----------------------------------------------------

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
  plot_econ_post <- 
    effects_economy_post  %>%  # Copy name effects
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
    labs(color = "**Economy Worsening (During)**") +
    ylab("Loneliness") +
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
          # axis.title.y = element_blank(),
          legend.margin = margin(t = leg_dist)
    ) + 
    coord_cartesian(ylim = c(lim_inf,lim_sup)) +
    guides(colour = guide_legend(title.position = "top", 
                                 title.hjust = .5, ncol = 1)) +
    labs(color = "**Material<br>Deprivation  (Before)**") +
    ylab("Loneliness") +
    scale_color_manual(values = group.colors)
  
## ---- LIVING ALONE POST ----------------------------------------------------

  # Fit 
  Fit_livingalone_post <- lmer(loneliness ~ 0 + livingalone_post * Time + 
                                 (1 | ID_ECS), 
                                 data = DB_graphs, 
                                 weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_livingalone_post <- # Change name effects_var
    effect("livingalone_post:Time", Fit_livingalone_post) |> 
    as.data.frame()
  
  # plot
  plot_livingalone_post <- effects_livingalone_post |> # Copy name effects
    ggplot(aes(x = Time, y = fit, 
               group = livingalone_post, 
               color = livingalone_post)) + 
    geom_point(position = position_dodge(.5)) + 
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
                                 title.hjust = .5, nrow = 2)) +
    labs(color = "**Living Alone<br>(During)**")+
    ylab("Loneliness") +
    scale_color_manual(values = group.colors)  

## ---- PHYSICAL ACTIVITY PRE ----------------------------------------
  
  # Fit 
  Fit_physicalactivity_pre <- 
    lmer(loneliness ~ 0 + physicalactivity_pre * Time + (1 | ID_ECS), 
                                data = DB_graphs,
                                weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_physicalactivity_pre <-
    effect("physicalactivity_pre:Time", Fit_physicalactivity_pre) |>
    as.data.frame()
  
  # plot
  plot_physicalactivity_pre <- effects_physicalactivity_pre |>
    ggplot(aes(x = Time, y = fit,
               group = physicalactivity_pre, 
               color = physicalactivity_pre)) + 
    geom_point(position = position_dodge(.5)) + 
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
    labs(color = "**Physical Activity<br>(During)**")+
    ylab("Loneliness") +
    scale_color_manual(values = group.colors) 
  
## ---- PHYSICAL ACTIVITY PRE ----------------------------------------
  
  # Fit 
  Fit_physicalactivity_post <- 
    lmer(loneliness ~ 0 + physicalactivity_post * Time + (1 | ID_ECS), 
         data = DB_graphs,
         weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_physicalactivity_post <-
    effect("physicalactivity_post:Time", Fit_physicalactivity_post) |>
    as.data.frame()
  
  # plot
  plot_physicalactivity_post <- effects_physicalactivity_post |>
    ggplot(aes(x = Time, y = fit,
               group = physicalactivity_post, 
               color = physicalactivity_post)) + 
    geom_point(position = position_dodge(.5)) + 
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
    labs(color = "**Physical Activity<br>(During)**")+
    ylab("Loneliness") +
    scale_color_manual(values = group.colors) 
  
## ---- DEPRESSION PRE --------------------------------------------
  
  # Fit 
  Fit_depression_pre <- 
    lmer(loneliness ~ 0 + depression_pre * Time + (1 | ID_ECS), 
                            data = DB_graphs,
                            weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_depression_pre <- # Change name effects_var
    effect("depression_pre:Time", Fit_depression_pre) |> # Change Here [2]
    as.data.frame()
  
  # plot
  plot_depression_pre <- 
    effects_depression_pre |> 
    ggplot(aes(x = Time, y = fit, 
               group = depression_pre, 
               color = depression_pre)) + 
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
    labs(color = "**Depression<br>(Before)**")+
    scale_color_manual(values = group.colors) 
  
## ---- DEPRESSION POST --------------------------------------------
  
  # Fit 
  Fit_depression_post <- 
    lmer(loneliness ~ 0 + depression_post * Time + (1 | ID_ECS), 
         data = DB_graphs,
         weights = DB_graphs %>% pull(weights))
  
  # Effects
  effects_depression_post <- # Change name effects_var
    effect("depression_post:Time", Fit_depression_post) |> # Change Here [2]
    as.data.frame()
  
  # Plot
  plot_depression_post <- 
    effects_depression_post |> 
    ggplot(aes(x = Time, y = fit, 
               group = depression_post, 
               color = depression_post)) + 
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

## ---- NEUROTICISM PRE ----------------------------------------------------
  
  # Mew graph
  plot_neuroticism_pre <- DB_graphs  %>%  
    ggplot(aes(x = neuroticism_pre, 
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Neuroticism") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 
  
  
## ---- EXTRAVERSION PRE ----------------------------------------------------
  
  plot_extraversion_pre <- DB_graphs |> 
    ggplot(aes(x = extraversion_pre, # [Change here]
               y = loneliness,
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Extraversion") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 
  
## ---- DISABILITY PRE ----------------------------------------------------

  plot_disability_pre <-
    DB_graphs |> # [Change here]
    ggplot(aes(x = disability_pre,
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Disability (Before)") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 


## ---- DISABILITY POST ----------------------------------------------------
  
  plot_disability_post <-
    DB_graphs |> # [Change here]
    ggplot(aes(x = disability_post,
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Disability (During)") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 

## ---- RESILIENCE POST ----------------------------------------------------
  
  plot_resilience_post <-
    DB_graphs |> 
    ggplot(aes(x = resilience_post,
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Resilience (During)") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 

## ---- SOCIAL SUPPORT PRE -------------------------------------------------

  plot_socialsupport_pre <-
    DB_graphs |> 
    ggplot(aes(x = socialsupport_pre,
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Social Support (Before)") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 

## ---- SOCIAL SUPPORT POST -------------------------------------------------
  
  plot_socialsupport_post <-
    DB_graphs |> 
    ggplot(aes(x = socialsupport_post,
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Social Support (During)") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 
  
## ---- WELLBEING CANTRIL PRE -------------------------------------------------
  
  plot_wellbeingcantril_pre <-
    DB_graphs |> 
    ggplot(aes(x = wellbeingcantril_pre,
               y = loneliness, 
               color = Time)) + 
    geom_smooth(formula = "y ~ x", method = "lm") +
    scale_color_manual(values = group.colors) +
    xlab("Wellbeing Cantril (Before)") + ylab("Loneliness") +
    coord_cartesian(ylim = c(lim_inf, lim_sup)) 
  
  

# LONELINESS TOTAL
  
# plot_total
  
## ---- MERGE SOCIODEMOGRAPHICS ---------------------------------

  # Add spacing between row subplots
  Spacing <- theme(plot.margin = unit(c(0,0,50,0), "pt"))
  plot_total <- plot_total + Spacing
  plot_age %<>% + Spacing
  plot_sex %<>% + Spacing
  plot_marital %<>% + Spacing
  plot_ed %<>% + Spacing
  plot_material_pre %<>% + Spacing
  
  # Plot merge 
  # plots_sociod <- 
  #   (plot_age + plot_sex) /
  #   (plot_marital + plot_ed) /
  #   (plot_material_pre + plot_unemployment_post) / 
  #   (plot_econ_post + plot_spacer())
  
  plots_sociod <- 
    (plot_total + plot_age) /
    (plot_sex + plot_marital) /
    (plot_ed + plot_material_pre) / 
    (plot_unemployment_post + plot_econ_post)
  
## ---- MERGE SOCIAL ASPECTS ---------------------------------
  
  # Add spacing between row subplots
  plot_socialsupport_pre %<>% + Spacing
  plot_socialsupport_post %<>% + Spacing
  plot_livingalone_post %<>% + Spacing
  plot_virtual %<>% + Spacing

  # Plot
  plots_social <- 
    (plot_socialsupport_pre + plot_socialsupport_post) /
    (plot_livingalone_post + plot_virtual) /
    (plot_social_changes + plot_spacer())
  
# ----- MERGE HEALTH AND WELLBEING ---------------------------------------------

  # Add spacing between row subplots
  plot_depression_pre %<>% + Spacing
  plot_depression_post %<>% + Spacing
  plot_physicalactivity_pre %<>% + Spacing
  plot_physicalactivity_post %<>% + Spacing
  plot_disability_pre %<>% + Spacing
  plot_disability_post %<>% + Spacing
  plot_neuroticism_pre %<>% + Spacing
  plot_extraversion_pre %<>% + Spacing
  
  # PLOT
  plots_health <- 
    (plot_depression_pre + plot_depression_post) / 
    (plot_physicalactivity_pre + plot_physicalactivity_post) /
    (plot_disability_pre + plot_disability_post) /
    (plot_neuroticism_pre + plot_extraversion_pre) /
    (plot_resilience_post + plot_wellbeingcantril_pre)
