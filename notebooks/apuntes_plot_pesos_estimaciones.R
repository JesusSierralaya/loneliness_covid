# Gráficos desde pesos
# 4 julio

# Modelo de efectos fijos
Fit_null <- lmer(Loneliness ~ 0 + Assessment + (1 | ID_ECS), 
                 data = db_pivot_2, 
                 weights = db_pivot_2$wfinal_norm)

# paquete effects
# Solo Assessment

ef_1 <- effect("Assessment", Fit_null) |> as.data.frame()

ef_1 |> ggplot(aes(x = Assessment, y = fit)) + 
  geom_point() + geom_errorbar(aes(ymin = lower, ymax = upper))

# Assessment con edad_cat
# Modelo mixto Fit_2
Fit2 <- lmer(Loneliness ~ 0 + q1011_age_cat * Assessment + (1 | ID_ECS), 
             data = db_pivot_2)

# Falta reorganizar el orden de los niveles
# Con Fit 2
ef_2 <- effect("q1011_age_cat:Assessment", Fit2) |> as.data.frame()

ef_2 |> ggplot(aes(x = Assessment, y = fit, 
                   group = q1011_age_cat, color = q1011_age_cat)) + 
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.5)) 