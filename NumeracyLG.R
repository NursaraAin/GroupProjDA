library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

num=read.csv("Numeracy.csv")
dt4=subset(num,select = c(reg,develop,cat,
                           Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                           Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt4))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt4)), rep(1, 0.3 * nrow(dt4))))
table(split1)
train=dt4[split1==0,]
test=dt4[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dt4Pred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dt4Pred)
#[1] 1.833684

#model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt4)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.2339 -0.9878  0.0948  0.9286  6.7373 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             -829.43432 1481.68337  -0.560    0.580    
# reg                                                        0.04618    0.38492   0.120    0.905    
# develop                                                    0.31325    0.93623   0.335    0.740    
# cat                                                       -0.83863    1.10780  -0.757    0.455    
# Total.Point.estimate                                       5.31145    0.74679   7.112 5.43e-08 ***
#   Children.without.functional.difficulties.Point.estimate   -4.38089    0.71921  -6.091 9.47e-07 ***
#   Time.period                                                0.41155    0.73425   0.561    0.579    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.07 on 31 degrees of freedom
# Multiple R-squared:  0.976,	Adjusted R-squared:  0.9713 
# F-statistic: 210.1 on 6 and 31 DF,  p-value: < 2.2e-16



plot(model,2)
#require regression for tpe
dt4_tpe=subset(train,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt4_tpe)
summary(model_tpe)
# Call:
#   lm(formula = Total.Point.estimate ~ ., data = dt4_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -34.368  -5.392   1.149   7.318  20.781 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -20753.443   4704.480  -4.411 0.000103 ***
#   reg             -3.832      1.181  -3.246 0.002688 ** 
#   develop          8.199      3.438   2.385 0.022992 *  
#   cat              8.445      4.089   2.065 0.046820 *  
#   Time.period     10.291      2.330   4.416 0.000102 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.29 on 33 degrees of freedom
# Multiple R-squared:  0.6746,	Adjusted R-squared:  0.6351 
# F-statistic:  17.1 on 4 and 33 DF,  p-value: 1.095e-07
plot(model_tpe,2)

#require regression for cwithout
dt4_cthout=subset(train,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt4_cthout)
summary(model_cthout)
# Call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt4_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -35.181  -5.958   0.652   7.887  22.708 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -21489.187   4884.906  -4.399 0.000107 ***
#   reg             -3.718      1.226  -3.033 0.004695 ** 
#   develop          8.346      3.570   2.338 0.025598 *  
#   cat              8.445      4.246   1.989 0.055034 .  
# Time.period     10.656      2.420   4.404 0.000106 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.76 on 33 degrees of freedom
# Multiple R-squared:  0.662,	Adjusted R-squared:  0.621 
# F-statistic: 16.16 on 4 and 33 DF,  p-value: 2.015e-07
plot(model_cthout,2)

#create prediction of comp
hmph=subset(dt4,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred4=data.frame(
  year=c(2017:2020),
  compDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
i=0
j=0
#set how many year to predict
year=5

for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt4)) {
    #predict for each row
    re=dt4[i,]$reg
    dv=dt4[i,]$develop
    house=dt4[i,]$cat
    
    #set year to predict
    t=yearPred4[nrow(yearPred4),]$year+1
    
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
  meanPred=pred/nrow(dt4)
  #create data frame
  newPred=data.frame(
    year=yearPred4[nrow(yearPred4),]$year+1,
    anarDisabled=meanPred
  )
  #insert to yearPred4
  yearPred4[nrow(yearPred4) + 1,]=newPred
}


plot(yearPred4$year, yearPred4$compDisabled, col="red",type = "b", lty = 2, frame=FALSE,
     main = "Percentage of children with disabilities with minimum profiency of numeracy skill", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred4[1:4,]$year, yearPred4[1:4,]$compDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)
