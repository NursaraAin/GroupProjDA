library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

anar=read.csv("HousingANAR.csv")
dt=subset(anar,select = c(reg,develop,level,X,
                        Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                        Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt)), rep(1, 0.3 * nrow(dt))))
table(split1)
train=dt[split1==0,]
test=dt[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dtPred)
#[1] 2.458556

#model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = dt2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -60.860 -19.034   4.772  18.175  57.188 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                              6.555e+03  1.632e+04   0.402    0.691
# Region                                                   7.734e+00  4.934e+00   1.567    0.129
# Development.regions                                     -4.163e+00  1.003e+01  -0.415    0.682
# Total.Point.estimate                                    -9.846e-02  7.623e+00  -0.013    0.990
# Children.without.functional.difficulties.Point.estimate  9.061e-01  7.682e+00   0.118    0.907
# Time.period                                             -3.266e+00  8.085e+00  -0.404    0.690
# 
# Residual standard error: 36.58 on 26 degrees of freedom
# Multiple R-squared:  0.1535,	Adjusted R-squared:  -0.009284 
# F-statistic: 0.943 on 5 and 26 DF,  p-value: 0.47
plot(model,2)

#require regression for tpe
dt_tpe=subset(dt,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt_tpe)
summary(model_tpe)
# Call:
#   lm(formula = Total.Point.estimate ~ ., data = dt2_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -39.241  -3.477   3.370   9.278  21.234 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)         -6622.043   7543.680  -0.878  0.38751   
# Region                 -6.771      1.897  -3.569  0.00132 **
#   Development.regions     2.524      4.647   0.543  0.59132   
# Time.period             3.332      3.735   0.892  0.37997   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 17.16 on 28 degrees of freedom
# Multiple R-squared:  0.478,	Adjusted R-squared:  0.422 
# F-statistic: 8.545 on 3 and 28 DF,  p-value: 0.0003464
plot(model_tpe,2)

#require regression for cwithout
dt_cthout=subset(train,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt_cthout)
summary(model_cthout)
# Call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt2_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -39.656  -2.905   3.669   8.947  22.044 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)         -6456.140   7485.663  -0.862  0.39576   
# Region                 -6.614      1.883  -3.513  0.00152 **
#   Development.regions     2.344      4.611   0.508  0.61523   
# Time.period             3.250      3.706   0.877  0.38803   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 17.03 on 28 degrees of freedom
# Multiple R-squared:  0.4689,	Adjusted R-squared:  0.412 
# F-statistic: 8.241 on 3 and 28 DF,  p-value: 0.0004366

plot(model_cthout,2)

#create prediction of ANAR
hmph=subset(dt,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred=data.frame(
  year=c(2017:2020),
  anarDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
i=0
j=0
#set how many year to predict
year=5

for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt)) {
    #predict for each row
    re=dt[i,]$reg
    dv=dt[i,]$develop
    indi=dt[i,]$level
    house=dt[i,]$X
    
    #set year to predict
    t=yearPred[nrow(yearPred),]$year+1
    
    #set total estimate
    d0=data.frame(reg=re,develop=dv,level=indi,X=house,Time.period=t,interval = "confidence")
    tpe=as.double(predict(model_tpe,d0))
  
    #set cthout
    d1=data.frame(reg=re,develop=dv,level=indi,X=house,Time.period=t,interval = "confidence")
    cthout=as.double(predict(model_cthout,d1))
    
    newdata=data.frame(reg=re,
                       develop=dv,
                       level=indi,
                       X=house,
                       Total.Point.estimate=tpe,
                       Children.without.functional.difficulties.Point.estimate=cthout,
                       Time.period = t, interval = "confidence")
    
   pred=pred+as.double(predict(model,newdata))
  }
    meanPred=pred/nrow(dt)
    #create data frame
    newPred=data.frame(
      year=yearPred[nrow(yearPred),]$year+1,
      anarDisabled=meanPred
    )
    #insert to yearPred
    yearPred[nrow(yearPred) + 1,]=newPred
}


plot(yearPred$year, yearPred$anarDisabled, col="red",type = "b", lty = 2, frame=FALSE,
     main = "Percentage of children with disabilities attending to school", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred[1:4,]$year, yearPred[1:4,]$anarDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)
