library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

comp=read.csv("CompletionRate.csv")
dt2=subset(comp,select = c(reg,develop,cat,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt2))

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt2)), rep(1, 0.3 * nrow(dt2))))
table(split1)
train=dt2[split1==0,]
test=dt2[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dt2Pred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dt2Pred)
#[1] 3.672523

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt2)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = dt2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -12.5565  -1.3738   0.2188   2.1148   6.0268 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             1598.1098  1566.4485   1.020    0.317    
# reg                                                       -0.4640     0.9933  -0.467    0.644    
# develop                                                    0.8661     1.7755   0.488    0.630    
# cat                                                        1.0943     1.4333   0.764    0.452    
# Total.Point.estimate                                       5.3968     0.6260   8.621 3.09e-09 ***
#   Children.without.functional.difficulties.Point.estimate   -4.4597     0.6327  -7.049 1.41e-07 ***
#   Time.period                                               -0.7904     0.7748  -1.020    0.317    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.663 on 27 degrees of freedom
# Multiple R-squared:  0.9822,	Adjusted R-squared:  0.9782 
# F-statistic: 248.2 on 6 and 27 DF,  p-value: < 2.2e-16


plot(model,2)
#require regression for tpe
dt2_tpe=subset(train,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt2_tpe)
summary(model_tpe)
# Call:
#   lm(formula = Total.Point.estimate ~ ., data = dt2_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -45.702  -8.841   1.787  14.291  24.651 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -10924.550  10080.990  -1.084    0.292
# reg             -6.964      6.607  -1.054    0.305
# develop          5.070     14.022   0.362    0.722
# cat             13.258      8.716   1.521    0.145
# Time.period      5.453      4.990   1.093    0.288
# 
# Residual standard error: 21.02 on 19 degrees of freedom
# Multiple R-squared:  0.3343,	Adjusted R-squared:  0.1942 
# F-statistic: 2.386 on 4 and 19 DF,  p-value: 0.08746
plot(model_tpe,2)

#require regression for cwithout
dt2_cthout=subset(train,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt2_cthout)
summary(model_cthout)
# Call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt2_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -45.370  -7.328   2.680  13.737  23.080 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -10719.081   9954.950  -1.077    0.295
# reg             -6.582      6.524  -1.009    0.326
# develop          5.944     13.847   0.429    0.673
# cat             13.218      8.607   1.536    0.141
# Time.period      5.350      4.928   1.086    0.291
# 
# Residual standard error: 20.76 on 19 degrees of freedom
# Multiple R-squared:  0.3364,	Adjusted R-squared:  0.1967 
# F-statistic: 2.408 on 4 and 19 DF,  p-value: 0.08525
plot(model_cthout,2)

#create prediction of comp
hmph=subset(dt2,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred2=data.frame(
  year=c(2017:2020),
  compDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
i=0
j=0
#set how many year to predict
year=5

for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt2)) {
    #predict for each row
    re=dt2[i,]$reg
    dv=dt2[i,]$develop
    house=dt2[i,]$cat
    
    #set year to predict
    t=yearPred2[nrow(yearPred2),]$year+1
    
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
  meanPred=pred/nrow(dt2)
  #create data frame
  newPred=data.frame(
    year=yearPred2[nrow(yearPred2),]$year+1,
    anarDisabled=meanPred
  )
  #insert to yearPred2
  yearPred2[nrow(yearPred2) + 1,]=newPred
}


plot(yearPred2$year, yearPred2$compDisabled, col="red",type = "b", lty = 2, frame=FALSE,
     main = "Percentage of children with disabilities completing primary school", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred2[1:4,]$year, yearPred2[1:4,]$compDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)

