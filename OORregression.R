setwd("C:/Users/nursa/OneDrive/GroupProjDA")
library(psych)
library(caret)
library(dplyr)
library(car)

oor=read.csv("totalOOR.csv")
dt1=subset(oor,select = c(regionN,Dvrregion,IndicatN,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt1))
pairs(cor(dt1))


ggplot(dt1, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Dvrregion))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )
ggplot(dt1, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(regionN))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )
ggplot(dt1, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(IndicatN))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )
ggplot(dt1, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Time.period))) +
  stat_smooth(method = "lm",
              col = "#C42126", se = FALSE, size = 1
  )

set.seed(5)
model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt1)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = dt1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -71.092 -11.987   1.615  14.548  58.649 
# 
# Coefficients:
#                                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             3415.5121  6653.0177   0.513   0.6090    
# regionN                                                    4.6039     1.8450   2.495   0.0144 *  
# Dvrregion                                                  4.6125     4.0560   1.137   0.2585    
# IndicatN                                                 -18.8861     3.9734  -4.753 7.65e-06 ***
# Total.Point.estimate                                       0.3148     0.3360   0.937   0.3515    
# Children.without.functional.difficulties.Point.estimate    0.1861     0.3197   0.582   0.5619    
# Time.period                                               -1.6807     3.2951  -0.510   0.6113    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 25.81 on 89 degrees of freedom
# Multiple R-squared:  0.5246,	Adjusted R-squared:  0.4925 
# F-statistic: 16.37 on 6 and 89 DF,  p-value: 1.295e-12

plot(model,2)
#require regression for tpe
dt1_tpe=subset(dt1,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt1_tpe)
summary(model_tpe)
plot(model_tpe,2)

#require regression for cwithout
dt1_cthout=subset(dt1,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt1_cthout)
summary(model_cthout)
plot(model_cthout,2)

#create prediction of OOR
hmph=subset(dt1,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred1=data.frame(
  year=c(2017:2020),
  oorDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
i=0
j=0
#set how many year to predict
year=5

for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt1)) {
    
    re=dt1[i,]$regionN
    dv=dt1[i,]$Dvrregion
    indi=dt1[i,]$IndicatN
    
    #set year to predict
    t=2020+j
    
    #set total estimate
    d0=data.frame(regionN=re,Dvrregion=dv,IndicatN=indi,Time.period=t,interval = "confidence")
    tpe=as.double(predict(model_tpe,d0))
    
    #set cthout
    d1=data.frame(regionN=re,Dvrregion=dv,IndicatN=indi,Time.period=t,interval = "confidence")
    cthout=as.double(predict(model_cthout,d1))
    
    newdata=data.frame(regionN=re,
                       Dvrregion=dv,
                       IndicatN=indi,
                       Total.Point.estimate=tpe,
                       Children.without.functional.difficulties.Point.estimate=cthout,
                       Time.period = t, interval = "confidence")
    
    pred=pred+as.double(predict(model,newdata))
  }
  meanPred=pred/nrow(dt1)
  #create data frame
  newPred=data.frame(
    year=2020+j,
    oorDisabled=meanPred
  )
  #insert to yearPred
  yearPred1[nrow(yearPred1) + 1,]=newPred
}


plot(yearPred1$year, yearPred1$oorDisabled, col="red",type = "b", lty = 2, frame=FALSE)
lines(yearPred1[1:4,]$year, yearPred1[1:4,]$oorDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)
