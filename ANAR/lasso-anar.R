#lasso regression

ANAR = read.csv("totalANAR.csv")
dt=subset(completionrate,select = c(regionN,Dvrregion,IndicatN,
                                    Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                                    Children.with.functional.difficulties.Point.estimate,Time.period))
str(dt)

y <- ANAR$Children.without.functional.difficulties.Point.estimate

x <- data.matrix(ANAR[, c('regionN', 'Dvrregion', 'IndicatN','Time.period')])

library(glmnet)

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