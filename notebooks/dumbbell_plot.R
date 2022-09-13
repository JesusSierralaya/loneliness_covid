
# here im going to try to draw a dumbbell plot
# im think this is the better way to represent paired data

# include
library(tidyverse)
library(here)
library(paint)
library(patchwork)
library(scales)
library(ggtext)

# 

# Database
source(here("throughput", "import_clean.R"))

# First im going to try this method, I think is more controlled

# https://youtu.be/qaksmQabMUI
# https://rpubs.com/mathetal/dumbbell

# parameters
color_before <- "#5dbae8"
color_during <- "#f47174"


Cat_properties <-   
  list(geom_line(color = "black"),
  geom_point(size = 5, alpha = 1),
  xlim(3, 9),
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_markdown(),
        axis.text.y = element_text(angle = 30)
        ),
  theme(legend.position="none"),
  scale_color_manual(
    values = c("Before" = color_before,
               "During"= color_during)) #,
    # scale_x_continuous(breaks = 3:9)
  ) 

Quant_propoerties <- 
  list(geom_smooth(formula = "y ~ x", method = "lm", show.legend = FALSE),
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_markdown(),
        axis.text.y = element_text(angle = 30)
        ),
  xlim(3, 9),
  theme(legend.position="none"),
  scale_color_manual(
    values = c("Before" = color_before,
               "During"= color_during))#,
  # scale_x_continuous(breaks = 3:9)
  )

# theme
theme_set(theme_bw())

# Variables -----------------------------------
# sex
sex <- DB_graphs %>% 
  group_by(sex, Time) %>% 
  summarise(Loneliness = mean(loneliness)) %>% 
  ggplot(aes(Loneliness, sex, 
             color = Time)) + 
  Cat_properties +
  theme(
    # legend.position="top",
    #     legend.title.align = 0.5,
    legend.position="none",
        axis.title.y = element_markdown(
          margin = 
            margin(t = 0, r = 0, b = 0, l = 30))
        )+
  # guides(
  #   colour = guide_legend(
  #     title.position = "top",
  #     title = "COVID-19 Lockdown")) +
  ylab("Sex")


# Depression pre
dep_pre <- DB_graphs %>% 
  group_by(depression_pre, Time) %>% 
  summarise(Loneliness = mean(loneliness)) %>% 
  ggplot(aes(Loneliness, depression_pre , 
             color = Time)) + 
  Cat_properties +
  ylab("Depression^*B*") +
  theme(axis.title.y = 
          element_markdown(margin = 
            margin(t = 0, r = 0, b = 0, l = 30))
  )

# Social changes
social_changes <- DB_graphs %>%
  drop_na(socialchanges_post) %>% 
  group_by(socialchanges_post, Time) %>% 
  summarise(Loneliness = mean(loneliness)) %>% 
  ggplot(aes(Loneliness, socialchanges_post , 
             color = Time)) + 
  Cat_properties +
  ylab("Social<br>Changes^*D*") +
  theme(axis.title.y = 
          element_markdown(margin = 
            margin(t = 0, r = 0, b = 0, l = 20))
  )

# Resilience
resilience <- DB_graphs %>% 
  ggplot(aes(x = loneliness,
             y = resilience_post, 
             color = Time)) + 
  Quant_propoerties +
  ylab("Resilience^*D*") +
  theme(axis.title.y = 
          element_markdown(margin = 
            margin(t = 0, r = 0, b = 0, l = 30))
  )

# Social support pre
socialsupport_pre <-
  DB_graphs |> 
  ggplot(aes(x = loneliness,
             y = socialsupport_pre, 
             color = Time)) + 
  Quant_propoerties +
  ylab("Social<br>Support^*B*") +
  theme(axis.title.y = 
          element_markdown(margin = 
            margin(t = 0, r = 0, b = 0, l = 20))
  )

# Social support post
socialsupport_post <-
  DB_graphs |> 
  ggplot(aes(x = loneliness,
             y = socialsupport_post, 
             color = Time)) + 
  Quant_propoerties +
  theme(axis.title.x=element_text(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line())+
  ylab("Social<br>Support^*D*") + 
  theme(axis.title.y = 
          element_markdown(margin = 
             margin(t = 0, r = 0, b = 0, l = 20))
  ) +
  # BOTTOM GRAPH
  scale_x_continuous(breaks = 3:9)+
  xlab("UCLA Loneliness Scale")

# LIVING ALONE
liv_alone <- DB_graphs %>% 
  group_by(livingalone_post, Time) %>% 
  summarise(Loneliness = mean(loneliness)) %>% 
  ggplot(aes(Loneliness, livingalone_post , 
             color = Time)) + 
  Cat_properties +
  ylab("Living Alone^*D*") +
  theme(axis.title.y = 
          element_markdown(margin = 
                             margin(t = 0, r = 0, b = 0, l = 30))
  ) +
  scale_y_discrete(position = "right")
# Merge graphs

(sex + liv_alone)/
 ( dep_pre + plot_spacer())/
  (social_changes + plot_spacer())/
  (resilience + plot_spacer())/
  (socialsupport_pre + plot_spacer())/
  (socialsupport_post  + plot_spacer())  +
  # https://patchwork.data-imaginist.com/articles/guides/annotation.html
  plot_annotation(
    title = 'The surprising truth about mtcars',
    subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
    caption = "*B*: Before the lockdown<br>
    *D*: During the lockdown"
  ) + 
  # https://patchwork.data-imaginist.com/reference/plot_layout.html
  plot_layout(guides='collect') &
  theme(legend.position='top') &
  guides(
  colour = guide_legend(
    title.position = "top",
    title = "COVID-19 Lockdown")) 

  

  # labs(
  #   caption = 
  #        "*B*: Before the lockdown<br>
  #         *D*: During the lockdown")  +
  # theme(
  #   # plot.title.position = "plot",
  #   plot.caption = element_markdown(hjust = 0),
  #   plot.caption.position = "plot"
  # )
