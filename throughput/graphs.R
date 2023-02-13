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
library(lme4)
library(ggtext)
library(patchwork)
library(magrittr)

# source("throughput/import_clean.R", encoding = 'UTF-8')

## ---- PRE-PROCESS: -----------------------------------------------------------

# Axis limits
lim_inf <- 3.1
lim_sup <- 6.8

# Legend
leg_dist <- 0

# Features
letter_size <- 18
font <- "serif"
point_size <- 3
errorbar_size <- 1.2

# Theme
theme_set(theme_minimal())

# color groups
group.colors <- c("#4070AE",
                  "#D13D39",
                  "#C39C11",
                  "#3F706D",
                  "#705A81")

## ---- LONELINESS TOTAL ----------------------------------------------------

# Fit
Fit_total <- lmer(
  loneliness ~ 0 + Time + (1 | ID_ECS),
  data = DB_graphs,
  weights = DB_graphs %>% pull(weights)
)

# Effects
effects_total <- effect("Time", Fit_total) |> as.data.frame()

# parameters
tittle_pos_y <- 1#1#-15

# plot
plot_total <- effects_total |>
  ggplot(aes(x = Time, y = fit)) +
  # This stay the same
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +

  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  ggtitle("Total Loneliness") +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = .5, 
                                  vjust = tittle_pos_y, 
                                  face = "bold")
        )

## ---- AGE ----------------------------------------------------

# Fit
Fit_age <- lmer(
  loneliness ~ 0 + age * Time + (1 | ID_ECS),
  data = DB_graphs,
  weights = DB_graphs %>% pull(weights)
)

# Effects
effects_age_cat <- effect("age:Time", Fit_age) |>
  as.data.frame()

# parameters
tittle_pos_y <- 1#2#8
legend_pos_y <- .95 # positive go up
legend_lvl_cols <- 2

# plot age
plot_age <- effects_age_cat %>%
  ggplot(aes(
    x = Time,
    y = fit,
    group = age,
    color = age
  )) +
  # This stay the same
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  #
  ylab("Loneliness") +
  #
  scale_color_manual(values = group.colors) +
  ggtitle("Age groups") +
    # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))


## ---- SEX ----------------------------------------------------

# Fit
Fit_sex <- lmer(
  loneliness ~ 0 + sex * Time + (1 | ID_ECS),
  data = DB_graphs,
  weights = DB_graphs %>% pull(weights)
)

# Effects
effects_sex <- effect("sex:Time", Fit_sex) |> as.data.frame()

# parameters
tittle_pos_y <- 2
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot sex
plot_sex <- effects_sex |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = sex,
    color = sex
  )) +
  geom_point(position = position_dodge(.5),  
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  # labs(color = "**Sex**")  +
  scale_color_manual(values = group.colors) +
  ggtitle("Sex") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- EDUCACION LEVEL ----------------------------------------------------

# Fit
Fit_educlevel <-
  lmer(
    loneliness ~ 0 + educlevel * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_educlevel <-
  effect("educlevel:Time", Fit_educlevel) %>% as.data.frame()

# parameters
tittle_pos_y <- 2
legend_pos_y <- 1
legend_lvl_cols <- 2

# plot education level
plot_ed <- effects_educlevel |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = educlevel,
    color = educlevel
  )) +
  geom_point(position = position_dodge(.5), size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Education level (Before)") +
  ylab("Loneliness") +
  # colors
  scale_color_manual(values = group.colors) +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- MARITAL STATUS ----------------------------------------------------

# Fit
Fit_marital <-
  lmer(
    loneliness ~ 0 + maritalstatus * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_marital <-
  effect("maritalstatus:Time", Fit_marital) |>
  as.data.frame()

# parameters
tittle_pos_y <- 5 #3 # 2.5
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 1

# plot marital estatus
plot_marital <- effects_marital |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = maritalstatus,
    color = maritalstatus
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  ggtitle("Marital status") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- VIRTUAL CONTACT POST --------------------------------------------------

# Fit
Fit_virtual <-
  lmer(
    loneliness ~ 0 + virtualcontact_post * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_virtual <-
  effect("virtualcontact_post:Time", Fit_virtual)  %>%
  as.data.frame()


# parameters
tittle_pos_y <- 2 # 5: 3 lines
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot virtual contact 
plot_virtual <- effects_virtual |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = virtualcontact_post,
    color = virtualcontact_post
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  ggtitle("Virtual contact (During)") +
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- SOCIAL CHANGES POST (SOLO3) ------------------------

# Fit
Fit_social_changes <-
  lmer(
    loneliness ~ 0 + socialchanges_post * Time +
      (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_social_changes <-
  effect("socialchanges_post:Time", Fit_social_changes) %>%
  as.data.frame()

# parameters
tittle_pos_y <- 2 #5 # 5: 3 lines
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 3

# plot social changes
plot_social_changes <- effects_social_changes  %>%
  ggplot(aes(
    x = Time,
    y = fit,
    group = socialchanges_post,
    color = socialchanges_post
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  ggtitle("Social changes (During)") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))


## ---- ECONOMY WORSENED POST ----------------------------------------------

# Fit
Fit_economy_post <-
  lmer(
    loneliness ~ 0 + economyworsened_post * Time +
      (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_economy_post <- # Change name effects_var
  effect("economyworsened_post:Time", Fit_economy_post) |> # Change Here [2]
  as.data.frame()

# parameters
tittle_pos_y <- 2 # 5: 3 lines
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot
plot_econ_post <-
  effects_economy_post  %>%  # Copy name effects
  ggplot(aes(
    x = Time,
    y = fit,
    group = economyworsened_post,
    color = economyworsened_post
  )) +
  # This stay the same
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  ggtitle("Worsening in economy (During)") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- UNEMPLOYMENT POST ----------------------------------------------

# Fit
Fit_unemployment_post <-
  lmer(
    loneliness ~ 0 + unemployment_post * Time +
      (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_unemployment_post <- # Change name effects_var
  effect("unemployment_post:Time", Fit_unemployment_post) |>
  as.data.frame()

# parameters
tittle_pos_y <- 2
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot unemployment
plot_unemployment_post <-
  effects_unemployment_post |> # Copy name effects
  ggplot(aes(
    x = Time,
    y = fit,
    group = unemployment_post,
    color = unemployment_post
  )) +
  # This stay the same
  geom_point(position = position_dodge(.5), 
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  theme(
    legend.position = "top",
    legend.title = element_markdown(),
    axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    legend.margin = margin(t = leg_dist)
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  scale_color_manual(values = group.colors) +
  ggtitle("Unemployment (During)") +
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- MATERIAL DEPRIVATION ------------------------------------------

# Fit
Fit_material_pre <-
  lmer(
    loneliness ~ 0 + materialdeprivation_pre * Time +
      (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_material_pre <- # Change name effects_var
  effect("materialdeprivation_pre:Time", Fit_material_pre) |>
  as.data.frame()

# # plot
# plot_material_pre <- effects_material_pre |> # Copy name effects
#   ggplot(aes(
#     x = Time,
#     y = fit,
#     group = materialdeprivation_pre,
#     color = materialdeprivation_pre
#   )) +
#   # This stay the same
#   geom_point(position = position_dodge(.5)) +
#   geom_errorbar(aes(ymin = lower, ymax = upper),
#                 position = position_dodge(.5),
#                 width = .2) +
#   theme(
#     legend.position = "top",
#     legend.title = element_markdown(),
#     axis.title.x = element_blank(),
#     legend.margin = margin(t = leg_dist),
#     legend.title.align = .5,
#   ) +
#   coord_cartesian(ylim = c(lim_inf, lim_sup)) +
#   guides(colour = guide_legend(
#     title.position = "top",
#     title.hjust = .5,
#     ncol = 2,
#     
#   )) +
#   labs(color = "**Material<br>deprivation  (Before)**") +
#   ylab("Loneliness") +
#   scale_color_manual(values = group.colors)

# parameters
tittle_pos_y <- 2
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot material deprivation
plot_material_pre <- 
  effects_material_pre |> # Copy name effects
  ggplot(aes(
    x = Time,
    y = fit,
    group = materialdeprivation_pre,
    color = materialdeprivation_pre
  )) +
  # This stay the same
  geom_point(position = position_dodge(.5), 
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Material deprivation (Before)") +
  ylab("Loneliness") +
  # colors
  scale_color_manual(values = group.colors) +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- LIVING ALONE POST ----------------------------------------------------

# Fit
Fit_livingalone_post <-
  lmer(
    loneliness ~ 0 + livingalone_post * Time +
      (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_livingalone_post <- # Change name effects_var
  effect("livingalone_post:Time", Fit_livingalone_post) |>
  as.data.frame()

# parameters
tittle_pos_y <- 2 # 5: 3 lines
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot living alone
plot_livingalone_post <-
  effects_livingalone_post |> # Copy name effects
  ggplot(aes(
    x = Time,
    y = fit,
    group = livingalone_post,
    color = livingalone_post
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  ggtitle("Living alone (During)") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- PHYSICAL ACTIVITY PRE ----------------------------------------

# Fit
Fit_physicalactivity_pre <-
  lmer(
    loneliness ~ 0 + physicalactivity_pre * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_physicalactivity_pre <-
  effect("physicalactivity_pre:Time", Fit_physicalactivity_pre) |>
  as.data.frame()

# parameters
tittle_pos_y <- 2 # 5: 3 lines
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 3

# plot physical act before
plot_physicalactivity_pre <- effects_physicalactivity_pre |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = physicalactivity_pre,
    color = physicalactivity_pre
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  ggtitle("Physical activity (Before)") +
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- PHYSICAL ACTIVITY PRE ----------------------------------------

# Fit
Fit_physicalactivity_post <-
  lmer(
    loneliness ~ 0 + physicalactivity_post * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_physicalactivity_post <-
  effect("physicalactivity_post:Time", Fit_physicalactivity_post) |>
  as.data.frame()

# parameters
tittle_pos_y <- 2 # 5: 3 lines
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 3

# plot
plot_physicalactivity_post <- effects_physicalactivity_post |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = physicalactivity_post,
    color = physicalactivity_post
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ylab("Loneliness") +
  scale_color_manual(values = group.colors) +
  ggtitle("Physical activity (During)") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- DEPRESSION PRE --------------------------------------------

# Fit
Fit_depression_pre <-
  lmer(
    loneliness ~ 0 + depression_pre * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_depression_pre <- # Change name effects_var
  effect("depression_pre:Time", Fit_depression_pre) |> # Change Here [2]
  as.data.frame()

# parameters
tittle_pos_y <- 2 # 5: 3 lines
legend_pos_y <- 1.03 # positive go up
legend_lvl_cols <- 2

# plot depression pre
plot_depression_pre <-
  effects_depression_pre |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = depression_pre,
    color = depression_pre
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  scale_color_manual(values = group.colors) +
  ylab("Loneliness") +
  ggtitle("Depression (Before)") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- DEPRESSION POST --------------------------------------------

# Fit
Fit_depression_post <-
  lmer(
    loneliness ~ 0 + depression_post * Time + (1 | ID_ECS),
    data = DB_graphs,
    weights = DB_graphs %>% pull(weights)
  )

# Effects
effects_depression_post <- # Change name effects_var
  effect("depression_post:Time", Fit_depression_post) |> # Change Here [2]
  as.data.frame()

# # parameters
# tittle_pos_y <- 1 # 5: 3 lines
# legend_pos_y <- .95 # positive go up
# legend_lvl_cols <- 2

# Plot depression post
plot_depression_post <-
  effects_depression_post |>
  ggplot(aes(
    x = Time,
    y = fit,
    group = depression_post,
    color = depression_post
  )) +
  geom_point(position = position_dodge(.5),
             size = point_size) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = .2,
                size = errorbar_size) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  labs(color = "**Depression<br>(During)**") +
  scale_color_manual(values = group.colors) +
  ggtitle("Depression (During)") +
  # Theme
  theme(
    # tittles
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = .5, 
                              vjust = tittle_pos_y,
                              face = "bold"),
    # legend
    legend.title = element_blank(),
    legend.position = c(.5, legend_pos_y),
    # text = element_text(size = 30)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- NEUROTICISM PRE ----------------------------------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# Plot neuroticism
plot_neuroticism_pre <-
  DB_graphs  %>%
  ggplot(aes(x = neuroticism_pre,
             y = loneliness,
             color = Time, weight = weights)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Neuroticism") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Neuroticism (Before)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))


## ---- EXTRAVERSION PRE ----------------------------------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# Plot extraversion
plot_extraversion_pre <-
  DB_graphs |>
  ggplot(aes(x = extraversion_pre, # [Change here]
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Extraversion") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Extraversion (Before)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- DISABILITY PRE ----------------------------------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot disability pre
plot_disability_pre <-
  DB_graphs |> # [Change here]
  ggplot(aes(x = disability_pre,
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Disability") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Disability (Before)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))


## ---- DISABILITY POST ----------------------------------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# plot disability post
plot_disability_post <-
  DB_graphs |> # [Change here]
  ggplot(aes(x = disability_post,
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Disability") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Disability (During)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- RESILIENCE POST ----------------------------------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# Plot resilience
plot_resilience_post <-
  DB_graphs |>
  ggplot(aes(x = resilience_post,
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Resilience") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Resilience (Before)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- SOCIAL SUPPORT PRE -------------------------------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# Plot social before
plot_socialsupport_pre <-
  DB_graphs |>
  ggplot(aes(x = socialsupport_pre,
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Social Support") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Social support (Before)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))
  

## ---- SOCIAL SUPPORT POST -------------------------------------------------

plot_socialsupport_post <-
  DB_graphs |>
  ggplot(aes(x = socialsupport_post,
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Social support") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  ggtitle("Social support (During)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))

## ---- EVALUATIVE WELLBEING CANTRIL PRE ----------------------------
# parameters
legend_pos_y <- 1 # positive go up
legend_lvl_cols <- 2

# Plot evaluative wellbeing
plot_wellbeingcantril_pre <-
  DB_graphs |>
  ggplot(aes(x = wellbeingcantril_pre,
             y = loneliness,
             color = Time)) +
  geom_smooth(formula = "y ~ x", method = "lm") +
  scale_color_manual(values = group.colors) +
  xlab("Evaluative wellbeing") + ylab("Loneliness") +
  coord_cartesian(ylim = c(lim_inf, lim_sup))+
  ggtitle("Evaluative wellbeing (Before)") +
  theme(plot.title = element_text(hjust = .5,
                                  face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.5, legend_pos_y)
  ) +
  # two columns levels 
  guides(colour = guide_legend(ncol = legend_lvl_cols))



# LONELINESS TOTAL

# properties

# Add spacing between row subplots
Spacing <- theme(plot.margin = unit(c(0, 0, 50, 0), "pt"))
Spacing_100 <- theme(plot.margin = unit(c(0, 0, 100, 0), "pt"))

# remove the y axis
no_axis_y <- theme(axis.title.y = element_blank(),
                   # Remove ticks and labels y
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())
# Remove legend (for quantitative variables)
no_legend <- theme(legend.position = "none")

## ---- MERGE SOCIODEMOGRAPHICS ---------------------------------

# Row 1
plot_total <- plot_total + Spacing 
plot_age <- plot_age + Spacing + no_axis_y

# Row 2
plot_sex <- plot_sex + Spacing + ylab("Loneliness")
plot_marital <- plot_marital + Spacing + no_axis_y

# Row 3
plot_ed <- plot_ed + Spacing + ylab("Loneliness")
plot_material_pre <- plot_material_pre + Spacing + no_axis_y

# Row 4 
plot_unemployment_post <- plot_unemployment_post + ylab("Loneliness")
plot_econ_post <- plot_econ_post + no_axis_y

# Plot merge

plots_sociod <-
  (plot_total + plot_age) /
  (plot_sex + plot_marital) /
  (plot_ed + plot_material_pre) /
  (plot_unemployment_post + plot_econ_post) & 
      theme(text = element_text(size = letter_size, family = font))    

## ---- MERGE SOCIAL ASPECTS ---------------------------------

# Add spacing between row subplots
# First row
plot_socialsupport_pre <- plot_socialsupport_pre + 
                          # no_legend + 
                          Spacing 
plot_socialsupport_post <- plot_socialsupport_post + no_axis_y + Spacing
# Second row
plot_livingalone_post <- plot_livingalone_post + Spacing
plot_virtual <- plot_virtual + no_axis_y
#& theme(text = element_text(size = letter_size))
# Third row
plot_social_changes <- plot_social_changes

# Plot
plots_social <-
  (plot_socialsupport_pre + plot_socialsupport_post) /
  (plot_livingalone_post + plot_virtual) /
  (plot_social_changes + plot_spacer()) & theme(text = element_text(size = letter_size, family = font))    

# ----- MERGE HEALTH AND WELLBEING -----------------------------------------

# Add spacing between row subplots
# First row
plot_depression_pre <- plot_depression_pre + Spacing
plot_depression_post <- plot_depression_post + no_axis_y + Spacing
# Second row
plot_physicalactivity_pre <- plot_physicalactivity_pre + Spacing
plot_physicalactivity_post <- plot_physicalactivity_post + no_axis_y + Spacing
# Third row
plot_disability_pre <- plot_disability_pre + 
                          # no_legend + 
                          Spacing
plot_disability_post <- plot_disability_post + no_axis_y + Spacing
# Fourth row
plot_neuroticism_pre <- plot_neuroticism_pre + 
                          # no_legend + 
                          Spacing
plot_extraversion_pre <- plot_extraversion_pre + no_axis_y  + Spacing
# fifth row
plot_resilience_post <- plot_resilience_post #+ no_legend
plot_wellbeingcantril_pre <- plot_wellbeingcantril_pre + no_axis_y

# PLOT
plots_health <-
  (plot_depression_pre + plot_depression_post) /
  (plot_physicalactivity_pre + plot_physicalactivity_post) /
  (plot_disability_pre + plot_disability_post) /
  (plot_neuroticism_pre + plot_extraversion_pre) /
  (plot_resilience_post + plot_wellbeingcantril_pre) & theme(text = element_text(size = letter_size, family = font))    
