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

fit.lm <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "lm", metric = metric, trControl = trainControl)
print(fit.lm)

fit.knn <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "knn", metric = metric, trControl = trainControl)
print(fit.knn)

fit.glmnet <- train(Children.with.functional.difficulties.Point.estimate~., data = train, method = "glmnet", metric = metric, trControl = trainControl)
print(fit.glmnet)

#compare
results <- resamples(list(LR = fit.lm, KNN = fit.knn, GLMNET = fit.glmnet))
summary(results)
dotplot(results)

# splitting dataset
validationIndex <- createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, p=0.80, list = FALSE)

# splitting dataset
validationIndex <- createDataPartition(dt$Children.with.functional.difficulties.Point.estimate, p=0.80, list = FALSE)
validation <- dt[-validationIndex,]
dt <- dt[validationIndex,]

#predictions
predictions <- as.numeric(predict(fit.lm, validation))
cor(predictions, validation$Children.with.functional.difficulties.Point.estimate)
#[1] 0.9979316
predictions <- as.numeric(predict(fit.knn, validation))
cor(predictions, validation$Children.with.functional.difficulties.Point.estimate)
#[1] 0.9907939
predictions <- as.numeric(predict(fit.glmnet, validation))
cor(predictions, validation$Children.with.functional.difficulties.Point.estimate)
#[1] 0.9891218
