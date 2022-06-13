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
dt3Pred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dt3Pred)
#[1] 3.658837

#model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt3)

summary(model)
# Call:
# lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#      ., data = dt3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -12.8582  -1.4760  -0.1036   1.6403   6.5856 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                              309.03582 1256.19319   0.246   0.8068    
# reg                                                       -0.03608    0.35119  -0.103   0.9186    
# develop                                                    1.81268    0.93976   1.929   0.0599 .  
# cat                                                       -0.62229    1.06780  -0.583   0.5629    
# Total.Point.estimate                                       4.64589    0.59376   7.825 5.37e-10 ***
#   Children.without.functional.difficulties.Point.estimate   -3.74475    0.58360  -6.417 6.86e-08 ***
#   Time.period                                               -0.15332    0.62230  -0.246   0.8065    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.478 on 46 degrees of freedom
# Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9668 
# F-statistic: 253.1 on 6 and 46 DF,  p-value: < 2.2e-16



plot(model,2)
#require regression for tpe
dt3_tpe=subset(train,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt3_tpe)
summary(model_tpe)
# Call:
#   lm(formula = Total.Point.estimate ~ ., data = dt3_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -24.151  -7.211   1.009   6.666  28.467 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -16369.998   4653.783  -3.518  0.00129 ** 
#   reg             -3.936      1.168  -3.370  0.00193 ** 
#   develop         13.521      3.401   3.976  0.00036 ***
#   cat             13.994      4.045   3.460  0.00151 ** 
#   Time.period      8.117      2.305   3.521  0.00128 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.16 on 33 degrees of freedom
# Multiple R-squared:  0.7095,	Adjusted R-squared:  0.6743 
# F-statistic: 20.15 on 4 and 33 DF,  p-value: 1.759e-08
plot(model_tpe,2)

#require regression for cwithout
dt3_cthout=subset(train,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt3_cthout)
summary(model_cthout)
# Call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt3_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -25.571  -6.766   1.615   6.980  27.691 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17066.098   4696.983  -3.633 0.000940 ***
#   reg             -3.799      1.179  -3.223 0.002857 ** 
#   develop         13.689      3.432   3.988 0.000348 ***
#   cat             14.385      4.082   3.524 0.001270 ** 
#   Time.period      8.462      2.327   3.637 0.000930 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.27 on 33 degrees of freedom
# Multiple R-squared:  0.7089,	Adjusted R-squared:  0.6736 
# F-statistic: 20.09 on 4 and 33 DF,  p-value: 1.826e-08

plot(model_cthout,2)

#create prediction of comp
hmph=subset(dt3,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred3=data.frame(
  year=c(2017:2020),
  compDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
i=0
j=0
#set how many year to predict
year=5

for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt3)) {
    #predict for each row
    re=dt3[i,]$reg
    dv=dt3[i,]$develop
    house=dt3[i,]$cat
    
    #set year to predict
    t=yearPred3[nrow(yearPred3),]$year+1
    
    #set total estimate
    d0=data.frame(reg=re,develop=dv,cat=house,Time.period=t,interval = "confidence")
    tpe=as.double(predict(model_tpe,d0))
    
    #set cthout
    d1=data.frame(reg=re,develop=dv,cat=house,Time.period=t,interval = "confidence")
    cthout=as.double(predict(model_cthout,d1))
    
    newdata=data.frame(reg=re,
                       develop=dv,
                       cat=house,
                       Total.Point.estimate=tpe,
                       Children.without.functional.difficulties.Point.estimate=cthout,
                       Time.period = t, interval = "confidence")
    
    pred=pred+as.double(predict(model,newdata))
  }
  meanPred=pred/nrow(dt3)
  #create data frame
  newPred=data.frame(
    year=yearPred3[nrow(yearPred3),]$year+1,
    anarDisabled=meanPred
  )
  #insert to yearPred3
  yearPred3[nrow(yearPred3) + 1,]=newPred
}


plot(yearPred3$year, yearPred3$compDisabled, col="red",type = "b", lty = 2, frame=FALSE,
     main = "Percentage of children with disabilities with minimum profiency of reading", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred3[1:4,]$year, yearPred3[1:4,]$compDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)

