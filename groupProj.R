#setwd("C:/Users/nursa/OneDrive/Documents/dataset")
library(psych)
library(caret)
library(dplyr)
library(car)
dt=read.csv("totalCompletionRate.csv")

corPlot(cor(dt))
pairs(cor(dt))

ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Development.regions))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )
ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Region))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )
ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Time.period))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )
set.seed(3)
model=lm(Children.with.functional.difficulties.Point.estimate~Development.regions,data = dt)
model2=lm(Children.with.functional.difficulties.Point.estimate~Region,data = dt)
model3=lm(Children.with.functional.difficulties.Point.estimate~Time.period,data = dt)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        Development.regions, data = dt)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -58.15 -37.52  11.81  29.52  49.16 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           58.145     15.290   3.803 0.000655 ***
#   Development.regions   -4.353      9.220  -0.472 0.640255    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 36.88 on 30 degrees of freedom
# Multiple R-squared:  0.007375,	Adjusted R-squared:  -0.02571 
# F-statistic: 0.2229 on 1 and 30 DF,  p-value: 0.6403
anova(model)
# Analysis of Variance Table
# 
# Response: Children.with.functional.difficulties.Point.estimate
#                     Df Sum Sq Mean Sq F value Pr(>F)
# Development.regions  1    303   303.2  0.2229 0.6403
# Residuals           30  40807  1360.2

summary(model2)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        Region, data = dt)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -57.246 -39.627   8.227  27.091  51.114 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   40.356     14.843   2.719   0.0108 *
#   Region         2.815      3.340   0.843   0.4060  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 36.59 on 30 degrees of freedom
# Multiple R-squared:  0.02313,	Adjusted R-squared:  -0.009432 
# F-statistic: 0.7104 on 1 and 30 DF,  p-value: 0.406
anova(model2)
# Analysis of Variance Table
# 
# Response: Children.with.functional.difficulties.Point.estimate
# Df Sum Sq Mean Sq F value Pr(>F)
# Region     1    951  950.91  0.7104  0.406
# Residuals 30  40159 1338.64 

summary(model3)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        Time.period, data = dt)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -52.846 -37.340   7.847  27.829  51.380 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  5729.942  14114.092   0.406    0.688
# Time.period    -2.813      6.993  -0.402    0.690
# 
# Residual standard error: 36.92 on 30 degrees of freedom
# Multiple R-squared:  0.005366,	Adjusted R-squared:  -0.02779 
# F-statistic: 0.1619 on 1 and 30 DF,  p-value: 0.6903
anova(model3)
# Analysis of Variance Table
# 
# Response: Children.with.functional.difficulties.Point.estimate
# Df Sum Sq Mean Sq F value Pr(>F)
# Time.period  1    221  220.61  0.1619 0.6903
# Residuals   30  40889 1362.98

plot(model,1)
plot(model,2)

plot(model2,1)
plot(model2,2)

plot(model3,1)
plot(model3,2)

newdata=data.frame(Development.regions = 1, interval = "confidence")
predict(model,newdata)

newdata=data.frame(Region = 1, interval = "confidence")
predict(model2,newdata)


newdata=data.frame(Time.period = 2022, interval = "confidence")
predict(model3,newdata)
