library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

anar=read.csv("ANAR/HousingANAR.csv")
dt=subset(anar,select = c(reg,develop,level,X,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt)), rep(1, 0.3 * nrow(dt))))
table(split1)

train=dt[split1==0,]
test=dt[split1==1,]

# algorithm building
trainControl <- trainControl(method = "cv", number = 10)
metric <- "RMSE"


fit.nb <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "nb", metric = metric, trControl = trainControl)
print(fit.nb)

fit.lm <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "lm", metric = metric, trControl = trainControl)
print(fit.lm)

fit.knn <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "knn", metric = metric, trControl = trainControl)
print(fit.knn)

fit.cart <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "rpart", metric = metric, trControl = trainControl)
print(fit.cart)

fit.glmnet <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "glmnet", metric = metric, trControl = trainControl)
print(fit.glmnet)

#compare
results <- resamples(list(NB = fit.nb, LR = fit.lm, KNN = fit.knn, CART = fit.cart, GLMNET = fit.glmnet))
summary(results)
dotplot(results)

# splitting dataset
validationIndex <- createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, p=0.80, list = FALSE)

# splitting dataset
validationIndex <- createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, p=0.80, list = FALSE)
validation <- dt[-validationIndex,]
dt <- dt[validationIndex,]

#predictions
predictions <- predict(fit.lm, validation)
conf_m <- confusionMatrix(predictions, validation$Children.with.functional.difficulties.Point.estimate)
print(conf_m)
pred_table <- table(predictions, validation$Children.with.functional.difficulties.Point.estimate)
print(pred_table)
