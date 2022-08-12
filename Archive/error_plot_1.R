library(ggplot2)

age_cat <- DB_2_pivot$age_cat
Loneliness <- DB_2_pivot$Loneliness
Evaluacion <- DB_2_pivot$Evaluacion

df <- data.frame(age_cat, Loneliness, Evaluacion)

###
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(df, varname="Loneliness", 
                    groupnames=c("Evaluacion", "age_cat"))
# Convert age_cat to a factor variable
df2$age_cat=as.factor(df2$age_cat)
head(df2)


###

p<- ggplot(df2, aes(x=age_cat, y=Loneliness, group=Evaluacion, color=Evaluacion)) + 
  geom_line() +
  geom_point(size = 2)+
  geom_errorbar(aes(ymin=Loneliness-sd, ymax=Loneliness+sd), width=.2, position = position_dodge(.05))

# ##
# # Finished line plot
# p+labs(title=" Loneliness per age_cat", x="Age groups", y = "Loneliness")+
#   theme_classic() +
#   scale_color_manual(values=c('blue','red'))

print(p)