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

#LINEAR REGRESSION
set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt)), rep(1, 0.3 * nrow(dt))))
table(split1)
train.lr=dt[split1==0,]
test.lr=dt[split1==1,]

model.lr=lm(Children.with.functional.difficulties.Point.estimate~.,data = train.lr)
dtPred=predict(model.lr,test.lr)

actuals_preds <- data.frame(cbind(actuals=test.lr$Children.with.functional.difficulties.Point.estimate, predicteds=dtPred))


mse.lr = mean((actuals_preds$actuals - actuals_preds$predicteds)^2)
mae.lr = MAE(actuals_preds$actuals, actuals_preds$predicteds)
rmse.lr = RMSE(actuals_preds$actuals, actuals_preds$predicteds)

cat("MSE: ", mse.lr, "MAE: ", mae.lr, " RMSE: ", rmse.lr)
#MSE:  6.044498 MAE:  1.898926  RMSE:  2.458556

cor(actuals_preds)
#              actuals predicteds
# actuals    1.0000000  0.9975991
# predicteds 0.9975991  1.0000000

summary(model.lr)

#----------
#KNN
indexes = createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, 
                              p = .85, list = F)

set.seed(2)
train.knn = dt[indexes, ]
test.knn = dt[-indexes, ]

train_x.knn = train.knn[, -6]
train_x.knn = scale(train_x.knn)[,]
train_y.knn = train.knn[,6]

test_x.knn = test.knn[, -6]
test_x.knn = scale(test.knn[,-6])[,]
test_y.knn = test.knn[,6]

model.knn = knnreg(train_x.knn, train_y.knn)
str(model.knn)

pred_y.knn = predict(model.knn, data.frame(test_x.knn))

print(data.frame(test_y.knn, pred_y.knn))

mse.knn = mean((test_y.knn - pred_y.knn)^2)
mae.knn = caret::MAE(test_y.knn, pred_y.knn)
rmse.knn = caret::RMSE(test_y.knn, pred_y.knn)

cat("MSE: ", mse.knn, "MAE: ", mae.knn, " RMSE: ", rmse.knn)
#:  43.53883 MAE:  4.03375  RMSE:  6.598396


#---------
#LASSO REGRESSION
library(glmnet)

dt.lasso=subset(anar,select = c(reg,develop,level,X,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
str(dt.lasso)

y.lasso <- anar$Children.without.functional.difficulties.Point.estimate

x.lasso <- data.matrix(anar[, c('reg', 'develop', 'level','X',
                                'Total.Point.estimate','Children.without.functional.difficulties.Point.estimate',
                                'Time.period')])


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x.lasso, y.lasso, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

#find coefficients of best model
best_model <- glmnet(x.lasso, y.lasso, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y.lasso_predicted <- predict(best_model, s = best_lambda, newx = x.lasso)

#find SST and SSE
sst <- sum((y.lasso - mean(y.lasso))^2)
sse <- sum((y.lasso_predicted - y.lasso)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq
# 0.9990716
hey=as.numeric(y.lasso_predicted)
preds <- data.frame(cbind(actuals=dt$Children.with.functional.difficulties.Point.estimate, predicteds=hey))

mse.lasso = mean((preds$actuals - preds$predicteds)^2)
mae.lasso = MAE(preds$actuals, preds$predicteds)
rmse.lasso = RMSE(preds$actuals, preds$predicteds)
cat("MSE: ", mse.lasso, "MAE: ", mae.lasso, " RMSE: ", rmse.lasso)
#MSE:  49.02464 MAE:  5.32957  RMSE:  7.00176

#------------
#COMPARISON

x = 1:length(actuals_preds$actuals)
x.lasso = 1:length(preds$actuals)
x.knn = 1:length(test_y.knn)
plot(x, actuals_preds$actuals, col = "red", type = "l", lwd=2,
     main = "attendance rate prediction")
#linear regression
lines(x, actuals_preds$predicteds, col = "blue", lwd=2)
#knn
lines(x.knn, pred_y.knn, col = "violet", lwd=2)
#lasso
lines(x.lasso, preds$predicteds, col = "green", lwd=2)
legend("bottomleft",  legend = c("original", "predicted-LinearRegression", "predicted-KNN", "predicted-LASSO"), 
       fill = c("red", "blue", "violet", "green"), col = 2:2,  adj = c(0, 0.6))
grid()


