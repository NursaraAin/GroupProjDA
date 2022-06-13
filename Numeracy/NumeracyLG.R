library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

num=read.csv("Numeracy.csv")
dt4=subset(num,select = c(reg,develop,cat,
                           Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                           Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt4))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt4)), rep(1, 0.3 * nrow(dt4))))
table(split1)
train=dt4[split1==0,]
test=dt4[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

actuals_preds <- data.frame(cbind(actuals=test$Children.with.functional.difficulties.Point.estimate, predicteds=dtPred))

mse = mean((actuals_preds$actuals - actuals_preds$predicteds)^2)
mae = MAE(actuals_preds$actuals, actuals_preds$predicteds)
rmse = RMSE(actuals_preds$actuals, actuals_preds$predicteds)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
#MSE:  3.362399 MAE:  1.570943  RMSE:  1.833684

x = 1:length(actuals_preds$actuals)
plot(x, actuals_preds$actuals, col = "red", type = "l", lwd=2,
     main = "anar lg data prediction")
lines(x, actuals_preds$predicteds, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
cor(actuals_preds)
#              actuals predicteds
# actuals    1.0000000  0.9904663
# predicteds 0.9904663  1.0000000

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.2339 -0.9878  0.0948  0.9286  6.7373 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             -829.43432 1481.68337  -0.560    0.580    
# reg                                                        0.04618    0.38492   0.120    0.905    
# develop                                                    0.31325    0.93623   0.335    0.740    
# cat                                                       -0.83863    1.10780  -0.757    0.455    
# Total.Point.estimate                                       5.31145    0.74679   7.112 5.43e-08 ***
#   Children.without.functional.difficulties.Point.estimate   -4.38089    0.71921  -6.091 9.47e-07 ***
#   Time.period                                                0.41155    0.73425   0.561    0.579    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.07 on 31 degrees of freedom
# Multiple R-squared:  0.976,	Adjusted R-squared:  0.9713 
# F-statistic: 210.1 on 6 and 31 DF,  p-value: < 2.2e-16