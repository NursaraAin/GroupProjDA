library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

anar=read.csv("ANAR/HousingANAR.csv")
dt=subset(anar,select = c(reg,develop,level,X,
                        Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                        Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt)), rep(1, 0.3 * nrow(dt))))
table(split1)
train=dt[split1==0,]
test=dt[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

actuals_preds <- data.frame(cbind(actuals=test$Children.with.functional.difficulties.Point.estimate, predicteds=dtPred))


mse = mean((actuals_preds$actuals - actuals_preds$predicteds)^2)
mae = MAE(actuals_preds$actuals, actuals_preds$predicteds)
rmse = RMSE(actuals_preds$actuals, actuals_preds$predicteds)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
#MSE:  6.044498 MAE:  1.898926  RMSE:  2.458556

x = 1:length(actuals_preds$actuals)
plot(x, actuals_preds$actuals, col = "red", type = "l", lwd=2,
     main = "anar lg data prediction")
lines(x, actuals_preds$predicteds, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
cor(actuals_preds)
#              actuals predicteds
# actuals    1.0000000  0.9975991
# predicteds 0.9975991  1.0000000

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = dt2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -60.860 -19.034   4.772  18.175  57.188 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                              6.555e+03  1.632e+04   0.402    0.691
# Region                                                   7.734e+00  4.934e+00   1.567    0.129
# Development.regions                                     -4.163e+00  1.003e+01  -0.415    0.682
# Total.Point.estimate                                    -9.846e-02  7.623e+00  -0.013    0.990
# Children.without.functional.difficulties.Point.estimate  9.061e-01  7.682e+00   0.118    0.907
# Time.period                                             -3.266e+00  8.085e+00  -0.404    0.690
# 
# Residual standard error: 36.58 on 26 degrees of freedom
# Multiple R-squared:  0.1535,	Adjusted R-squared:  -0.009284 
# F-statistic: 0.943 on 5 and 26 DF,  p-value: 0.47
