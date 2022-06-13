library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

anar=read.csv("ANAR/HousingANAR.csv")
dt=subset(anar,select = c(reg,develop,level,X,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
str(dt)

indexes = createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, 
                              p = .85, list = F)

set.seed(4)
train = dt[indexes, ]
test = dt[-indexes, ]

train_x = train[, -6]
train_x = scale(train_x)[,]
train_y = train[,6]

test_x = test[, -6]
test_x = scale(test[,-6])[,]
test_y = test[,6]

knnmodel = knnreg(train_x, train_y)
str(knnmodel)

pred_y = predict(knnmodel, data.frame(test_x))

print(data.frame(test_y, pred_y))

mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
#MSE:  80.29747 MAE:  7.76875  RMSE:  8.960886

x = 1:length(test_y)

plot(x, test_y, col = "red", type = "l", lwd=2,
     main = "anar knn data prediction")
lines(x, pred_y, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

cor(data.frame(test_y, pred_y))
#           test_y    pred_y
# test_y 1.0000000 0.9739955
# pred_y 0.9739955 1.0000000