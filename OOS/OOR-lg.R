library(psych)
library(caret)
library(dplyr)
library(car)

oor=read.csv("OOS/HousingOOR.csv")
dt1=subset(oor,select = c(reg,develop,level,X,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt1))

set.seed(8)
split1<- sample(c(rep(0, 0.7 * nrow(dt1)), rep(1, 0.3 * nrow(dt1))))
table(split1)
train=dt1[split1==0,]
test=dt1[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

actuals_preds <- data.frame(cbind(actuals=test$Children.with.functional.difficulties.Point.estimate, predicteds=dtPred))


mse = mean((actuals_preds$actuals - actuals_preds$predicteds)^2)
mae = MAE(actuals_preds$actuals, actuals_preds$predicteds)
rmse = RMSE(actuals_preds$actuals, actuals_preds$predicteds)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
#MSE:  5.156368 MAE:  1.64095  RMSE:  2.270764

x = 1:length(actuals_preds$actuals)
plot(x, actuals_preds$actuals, col = "red", type = "l", lwd=2,
     main = "anar lg data prediction")
lines(x, actuals_preds$predicteds, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
cor(actuals_preds)
# actuals predicteds
# actuals    1.0000000  0.9906422
# predicteds 0.9906422  1.0000000

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.2412 -1.7173 -0.6061  0.9210 10.6210 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             -536.2213   720.2423  -0.745    0.459    
# reg                                                       -0.2311     0.2999  -0.771    0.443    
# develop                                                   -0.6709     0.7290  -0.920    0.360    
# level                                                      0.4489     0.4004   1.121    0.266    
# X                                                          0.5994     0.6604   0.908    0.367    
# Total.Point.estimate                                       4.7735     0.3821  12.494  < 2e-16 ***
#   Children.without.functional.difficulties.Point.estimate   -3.8013     0.3869  -9.824  4.1e-15 ***
#   Time.period                                                0.2665     0.3566   0.747    0.457    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.707 on 75 degrees of freedom
# Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9763 
# F-statistic: 483.1 on 7 and 75 DF,  p-value: < 2.2e-16