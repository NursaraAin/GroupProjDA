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
split1<- sample(c(rep(0, 0.7 * nrow(dt1)), rep(1, 0.3 * nrow(dt1))))
table(split1)
train=dt1[split1==0,]
test=dt1[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dtPred)
#[1] 6.389404

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
# Call:
#   lm(formula = Total.Point.estimate ~ ., data = dt1_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -18.560  -8.372  -1.467   5.047  43.409 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1269.33260 3085.64396   0.411   0.6818    
# regionN        3.87480    0.77613   4.992 2.86e-06 ***
#   Dvrregion      0.04993    1.90077   0.026   0.9791    
# IndicatN       2.86875    1.51961   1.888   0.0622 .  
# Time.period   -0.63165    1.52765  -0.413   0.6802    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.16 on 91 degrees of freedom
# Multiple R-squared:  0.3201,	Adjusted R-squared:  0.2902 
# F-statistic: 10.71 on 4 and 91 DF,  p-value: 3.708e-07
plot(model_tpe,2)

#require regression for cwithout
dt1_cthout=subset(dt1,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt1_cthout)
summary(model_cthout)
# Call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt1_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -18.773  -8.270  -1.656   5.033  42.530 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1570.4936  3055.9831   0.514    0.609    
# regionN        3.8475     0.7687   5.005 2.71e-06 ***
#   Dvrregion      0.3626     1.8825   0.193    0.848    
# IndicatN       2.4781     1.5050   1.647    0.103    
# Time.period   -0.7810     1.5130  -0.516    0.607    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.04 on 91 degrees of freedom
# Multiple R-squared:  0.3174,	Adjusted R-squared:  0.2874 
# F-statistic: 10.58 on 4 and 91 DF,  p-value: 4.408e-07
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
    #predict for each row
    re=dt1[i,]$regionN
    dv=dt1[i,]$Dvrregion
    indi=dt1[i,]$IndicatN
    
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
  meanPred=pred/nrow(dt1)
  #create data frame
  newPred=data.frame(
    year=yearPred[nrow(yearPred),]$year+1,
    oorDisabled=meanPred
  )
  #insert to yearPred
  yearPred1[nrow(yearPred1) + 1,]=newPred
}


plot(yearPred1$year, yearPred1$oorDisabled, col="red",type = "b", lty = 2, frame=FALSE)
lines(yearPred1[1:4,]$year, yearPred1[1:4,]$oorDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)
