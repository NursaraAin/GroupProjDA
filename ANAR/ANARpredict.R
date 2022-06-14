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

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt)

#need to predict percentage total and normal children depending on year
#require regression for tpe
dt_tpe=subset(dt,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt_tpe)

#require regression for cwithout
dt_cthout=subset(dt,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt_cthout)

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
    tpe=dt[i,]$Total.Point.estimate
    cthout=dt[i,]$Children.without.functional.difficulties.Point.estimate
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


plot(yearPred$year, yearPred$anarDisabled, col="red",type = "b", lty = 2, frame=FALSE, lwd=2,
     main = "Percentage of children with disabilities attending to school", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred[1:4,]$year, yearPred[1:4,]$anarDisabled, col="blue",type = "b", lty = 1, lwd=2)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)

