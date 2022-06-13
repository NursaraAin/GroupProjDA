library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

baca=read.csv("Reading.csv")
dt3=subset(baca,select = c(reg,develop,cat,
                           Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                           Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt3))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt3)), rep(1, 0.3 * nrow(dt3))))
table(split1)
train=dt3[split1==0,]
test=dt3[split1==1,]

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
# actuals    1.0000000  0.9784178
# predicteds 0.9784178  1.0000000

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = train)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.3713  -0.7627   0.4866   2.1866   5.9321 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             -1.268e+03  1.667e+03  -0.761    0.453    
# reg                                                     -9.082e-02  4.167e-01  -0.218    0.829    
# develop                                                  1.736e+00  1.225e+00   1.416    0.167    
# cat                                                     -4.849e-01  1.413e+00  -0.343    0.734    
# Total.Point.estimate                                     5.346e+00  7.070e-01   7.561 1.60e-08 ***
#   Children.without.functional.difficulties.Point.estimate -4.469e+00  7.005e-01  -6.379 4.19e-07 ***
#   Time.period                                              6.288e-01  8.258e-01   0.761    0.452    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.599 on 31 degrees of freedom
# Multiple R-squared:  0.9722,	Adjusted R-squared:  0.9668 
# F-statistic: 180.7 on 6 and 31 DF,  p-value: < 2.2e-16