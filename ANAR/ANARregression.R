setwd("C:/Users/nursa/OneDrive/GroupProjDA")
library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

anar=read.csv("totalANAR.csv")
dt=subset(anar,select = c(regionN,Dvrregion,IndicatN,
                        Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                        Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt))
# pairs(cor(dt))
# 
# 
# ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Dvrregion))) +
#   stat_smooth(method = "lm",
#               col = "#C42126", se = FALSE, size = 1
#   )
# ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(regionN))) +
#   stat_smooth(method = "lm",
#               col = "#C42126", se = FALSE, size = 1
#   )
# ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(IndicatN))) +
#   stat_smooth(method = "lm",
#               col = "#C42126", se = FALSE, size = 1
#   )
# ggplot(dt, aes(x = log(Children.with.functional.difficulties.Point.estimate), y = log(Time.period))) +
#   stat_smooth(method = "lm",
#               col = "#C42126", se = FALSE, size = 1
#   )

set.seed(4)
split1<- sample(c(rep(0, 0.7 * nrow(dt)), rep(1, 0.3 * nrow(dt))))
table(split1)
train=dt[split1==0,]
test=dt[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dtPred)
#[1] 22.68196

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = dt)
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
dt_tpe=subset(dt,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt_tpe)
summary(model_tpe)
# Call:
#   lm(formula = Total.Point.estimate ~ ., data = dt_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -81.132  -7.802   2.073  15.234  37.323 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -8617.5520  5753.9282  -1.498    0.138    
# regionN        -6.7819     1.4473  -4.686 9.73e-06 ***
#   Dvrregion      -0.4206     3.5445  -0.119    0.906    
# IndicatN      -19.0844     2.8337  -6.735 1.44e-09 ***
#   Time.period     4.3331     2.8487   1.521    0.132    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 22.67 on 91 degrees of freedom
# Multiple R-squared:  0.4948,	Adjusted R-squared:  0.4726 
# F-statistic: 22.28 on 4 and 91 DF,  p-value: 7.569e-13
plot(model_tpe,2)

#require regression for cwithout
dt_cthout=subset(dt,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt_cthout)
summary(model_cthout)
# Call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -78.675  -7.834   2.443  16.118  39.733 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6759.4879  6048.7077  -1.118 0.266718    
# regionN        -6.0854     1.5214  -4.000 0.000129 ***
#   Dvrregion       0.8666     3.7260   0.233 0.816602    
# IndicatN      -20.1344     2.9789  -6.759 1.29e-09 ***
#   Time.period     3.4112     2.9946   1.139 0.257648    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 23.83 on 91 degrees of freedom
# Multiple R-squared:  0.4596,	Adjusted R-squared:  0.4358 
# F-statistic: 19.35 on 4 and 91 DF,  p-value: 1.513e-11
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
    re=dt[i,]$regionN
    dv=dt[i,]$Dvrregion
    indi=dt[i,]$IndicatN
    
    #set year to predict
    t=yearPred[nrow(yearPred),]$year+1
    
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
