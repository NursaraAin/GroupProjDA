completionrate=read.csv("totalCompletionRate.csv")
dt=subset(completionrate,select = c(regionN,Dvrregion,IndicatN,
                         Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                         Children.with.functional.difficulties.Point.estimate,Time.period))
str(dt)

indexes = createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, 
                              p = .85, list = F)

set.seed(12)

indexes2 = createDataPartition(boston$medv, p = .85, list = F)
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
#MSE:  38.47043 MAE:  4.958333  RMSE:  6.202454

x = 1:length(test_y)

plot(x, test_y, col = "red", type = "l", lwd=2,
     main = "KNN completion rate primary data prediction")
lines(x, pred_y, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

