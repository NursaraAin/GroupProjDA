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

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt2)

#require regression for tpe
dt2_tpe=subset(train,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt2_tpe)

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