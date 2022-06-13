library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

comp=read.csv("CompletionRate.csv")
dt2=subset(comp,select = c(reg,develop,cat,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt2))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt2)), rep(1, 0.3 * nrow(dt2))))
table(split1)
train=dt2[split1==0,]
test=dt2[split1==1,]


model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

actuals_preds <- data.frame(cbind(actuals=test$Children.with.functional.difficulties.Point.estimate, predicteds=dtPred))


mse = mean((actuals_preds$actuals - actuals_preds$predicteds)^2)
mae = MAE(actuals_preds$actuals, actuals_preds$predicteds)
rmse = RMSE(actuals_preds$actuals, actuals_preds$predicteds)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
#MSE:  13.48742 MAE:  2.817267  RMSE:  3.672523

x = 1:length(actuals_preds$actuals)
plot(x, actuals_preds$actuals, col = "red", type = "l", lwd=2,
     main = "anar lg data prediction")
lines(x, actuals_preds$predicteds, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
cor(actuals_preds)
#              actuals predicteds
# actuals    1.0000000  0.9953067
# predicteds 0.9953067  1.0000000

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.2736  -1.5735   0.8706   2.3930   4.8214 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             2191.3107  2065.1669   1.061    0.303    
# reg                                                        0.2608     1.3695   0.190    0.851    
# develop                                                    3.3535     2.8953   1.158    0.263    
# cat                                                        0.9085     1.8387   0.494    0.628    
# Total.Point.estimate                                       5.6245     0.7823   7.189 1.51e-06 ***
#   Children.without.functional.difficulties.Point.estimate   -4.7104     0.7923  -5.946 1.60e-05 ***
#   Time.period                                               -1.0865     1.0228  -1.062    0.303    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.178 on 17 degrees of freedom
# Multiple R-squared:  0.9773,	Adjusted R-squared:  0.9693 
# F-statistic:   122 on 6 and 17 DF,  p-value: 5.069e-13
