## Explicacion de porque la sd muestral es distinta 
# a la sd de un estimador 
set.seed(10)
n <- 10^2
x <- rnorm(n)

# Estadísticos
media <- mean(x)
sd(x)# desv estandar de la muestra
# Estimador 
sd(x)*sqrt(n/(n-1)) # estimador insesgado de la sd poblacional
# Error estandar
# Es menor al de la distribución o a las barras de error de la sd
error_est_media <- (sd(x)*sqrt(n/(n-1)))/sqrt(n) # error estandar del estimador de la media poblacional

# modelo lineal
library(tidyverse)
data <- data.frame(x)

data |> lm(formula = x~1) |> summary()
# Estos son los límites del estimador
lim_sup <- media + error_est_media*1.96
lim_inf <- media - error_est_media*1.96
# Estos son las distribuciones
sort(x)[3]
sort(x)[98]
