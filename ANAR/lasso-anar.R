#lasso regression
library(glmnet)

anar=read.csv("ANAR/HousingANAR.csv")
dt=subset(anar,select = c(reg,develop,level,X,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
str(dt)

y <- anar$Children.without.functional.difficulties.Point.estimate

x <- data.matrix(anar[, c('reg', 'develop', 'level','X',
                          'Total.Point.estimate','Children.without.functional.difficulties.Point.estimate',
                          'Time.period')])


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq
#0.4557332
hey=as.numeric(y_predicted)
preds <- data.frame(cbind(actuals=dt$Children.with.functional.difficulties.Point.estimate, predicteds=hey))

mse = mean((preds$actuals - preds$predicteds)^2)
mae = MAE(preds$actuals, preds$predicteds)
rmse = RMSE(preds$actuals, preds$predicteds)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
#MSE:  49.02464 MAE:  5.32957  RMSE:  7.00176

x = 1:length(preds$actuals)
plot(x, preds$actuals, col = "red", type = "l", lwd=2,
     main = "anar lasso data prediction")
lines(x, preds$predicteds, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
cor(preds)
#              actuals predicteds
# actuals    1.0000000  0.9837811
# predicteds 0.9837811  1.0000000