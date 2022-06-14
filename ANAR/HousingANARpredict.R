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

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt)

#need to predict percentage total and normal children depending on year
#require regression for tpe
dt_tpe=subset(dt,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt_tpe)

#require regression for cwithout
dt_cthout=subset(dt,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt_cthout)

#create prediction of ANAR
hmph=subset(dt,select = c(X,Time.period,Children.with.functional.difficulties.Point.estimate))
insertYear=function(opt){
  a=b=c=d=k=0
  e=f=g=h=0
  for(k in 1:nrow(hmph)){
    if(hmph[k,]$X==opt){
      if(hmph[k,]$Time.period==2017){
        a=a+hmph[k,]$Children.with.functional.difficulties.Point.estimate
        e=e+1
      }
      else if(hmph[k,]$Time.period==2018){
        b=b+hmph[k,]$Children.with.functional.difficulties.Point.estimate
        f=f+1
      }
      else if(hmph[k,]$Time.period==2019){
        c=c+hmph[k,]$Children.with.functional.difficulties.Point.estimate
        g=g+1
      }
      else{
        d=d+hmph[k,]$Children.with.functional.difficulties.Point.estimate
        h=h+1
      }
    }
  }
  a=a/e
  b=b/f
  c=c/g
  d=d/h
  data.frame(
    year=c(2017:2020),
    disabled=c(a,b,c,d)
  )
}
yearPred_R=insertYear(1)
yearPred_U=insertYear(2)

future=function(z,y,opt){
  i=0
  j=0
  #set how many year to predict
  year=y
  
  for(j in 1:year){
    pred=0
    for(i in 1:nrow(filter(dt,X == opt))) {
      #predict for each row
      re=dt[i,]$reg
      dv=dt[i,]$develop
      indi=dt[i,]$level
      house=dt[i,]$X
      
      #set year to predict
      t=z[nrow(z),]$year+1
      
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
      year=z[nrow(z),]$year+1,
      anarDisabled=meanPred
    )
    #insert to yearPred
    z[nrow(z) + 1,]=newPred
  }
  z
}
yearPred_R<-data.frame(future(yearPred_R,5,1))
yearPred_U<-data.frame(future(yearPred_U,5,2))

plot(yearPred_U$year, yearPred_U$disabled, col="red",type = "b", lty = 2, asp = 0.1, lwd=2,
     main = "Percentage of children with disabilities attending to school group by housing area", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred_U[1:4,]$year, yearPred_U[1:4,]$disabled, col="deeppink",type = "b", lty = 1,lwd=2)

lines(yearPred_R$year, yearPred_R$disabled, col="blue",type = "b", lty = 2,lwd=2)
lines(yearPred_R[1:4,]$year, yearPred_R[1:4,]$disabled, col="purple",type = "b", lty = 1,lwd=2)

legend("topright", legend=c("Predicted (Rural)", "Actual (Rural)","Predicted (Urban)", "Actual (Urban)"),
       col=c("red", "deeppink","blue","purple"), lty = 2:1, cex=0.8,lwd=2)

hmph=subset(dt,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred=data.frame(
  year=c(2017:2020),
  anarDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
year=5
i=j=0
for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt)) {
    #predict for each row
    re=dt[i,]$reg
    dv=dt[i,]$develop
    indi=dt[i,]$level
    house=1
    
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
     main = "Percentage of children with disabilities attending to school in rural", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred[1:4,]$year, yearPred[1:4,]$anarDisabled, col="blue",type = "b", lty = 1, lwd=2)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)

hmph=subset(dt,select = c(Time.period,Children.with.functional.difficulties.Point.estimate))
a=colMeans(hmph[hmph$Time.period=='2017',])
b=colMeans(hmph[hmph$Time.period=='2018',])
c=colMeans(hmph[hmph$Time.period=='2019',])
d=colMeans(hmph[hmph$Time.period=='2020',])

yearPred=data.frame(
  year=c(2017:2020),
  anarDisabled=c(as.double(a[2]),as.double(b[2]),as.double(c[2]),as.double(d[2]))
)
year=5
i=j=0
for(j in 1:year){
  pred=0
  for(i in 1:nrow(dt)) {
    #predict for each row
    re=dt[i,]$reg
    dv=dt[i,]$develop
    indi=dt[i,]$level
    house=2
    
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

lines(yearPred$year, yearPred$anarDisabled, col="deeppink",type = "b", lty = 2, lwd=2)
lines(yearPred[1:4,]$year, yearPred[1:4,]$anarDisabled, col="purple",type = "b", lty = 1, lwd=2)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)
