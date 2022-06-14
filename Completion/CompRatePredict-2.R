library(psych)
library(caret)
library(dplyr)
library(car)
library(Metrics)

comp=read.csv("Completion/CompletionRate.csv")
dt2=subset(comp,select = c(reg,develop,cat,
                           Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                           Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt2))

set.seed(4)

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt2)

#require regression for tpe
dt2_tpe=subset(dt2,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt2_tpe)
dt_cthout=subset(dt2,select = -c(Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt_cthout)

#create prediction of comp
insertYear=function(opt,col){
  a=b=c=d=k=0
  e=f=g=h=0
  
  for(k in 1:nrow(dt2)){
    if(dt2[k,col]==opt){
      if(dt2[k,]$Time.period==2017){
        a=a+dt2[k,]$Children.with.functional.difficulties.Point.estimate
        e=e+1
      }
      else if(dt2[k,]$Time.period==2018){
        b=b+dt2[k,]$Children.with.functional.difficulties.Point.estimate
        f=f+1
      }
      else if(dt2[k,]$Time.period==2019){
        c=c+dt2[k,]$Children.with.functional.difficulties.Point.estimate
        g=g+1
      }
      else{
        d=d+dt2[k,]$Children.with.functional.difficulties.Point.estimate
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

future=function(z,y,opt,col){
  i=0
  j=0
  #set how many year to predict
  year=y=5
  
  for(j in 1:year){
    pred=0
    len=nrow(filter(dt2,dt2[,col] == opt))
    for(i in 1:len) {
      #predict for each row
      re=dt2[i,]$reg
      dv=dt2[i,]$develop
      house=dt2[i,]$cat
      
      #set year to predict
      t=z[nrow(z),]$year+1
      
      #set total estimate
      d0=data.frame(reg=re,develop=dv,cat=house,Time.period=t,interval = "confidence")
      tpe=as.double(predict(model_tpe,d0))
      
      #set cthout
      d1=data.frame(reg=re,develop=dv,cat=house,Total.Point.estimate=tpe,Time.period=t,interval = "confidence")
      cthout=as.double(predict(model_cthout,d1))
      
      newdata=data.frame(reg=re,
                         develop=dv,
                         cat=house,
                         Total.Point.estimate=tpe,
                         Children.without.functional.difficulties.Point.estimate=cthout,
                         Time.period = t, interval = "confidence")
      pred=pred+as.double(predict(model,newdata))
    }
    meanPred=pred/len
    #create data frame
    newPred=data.frame(
      year=z[nrow(z),]$year+1,
      disabled=meanPred
    )
    #insert to yearPred
    z[nrow(z) + 1,]=newPred
  }
  z
}
#predict based on housing area
yearPred_R=insertYear(1,3)
yearPred_U=insertYear(2,3)

yearPred_R<-data.frame(future(yearPred_R,5,1,3))
yearPred_U<-data.frame(future(yearPred_U,5,2,3))

plot(yearPred_U$year, yearPred_U$disabled, col="red",type = "b", lty = 2, asp = 0.05, lwd=2,
     main = "Percentage of children with disabilities attending to school group by housing area", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred_U[1:4,]$year, yearPred_U[1:4,]$disabled, col="deeppink",type = "b", lty = 1,lwd=2)

lines(yearPred_R$year, yearPred_R$disabled, col="blue",type = "b", lty = 2,lwd=2)
lines(yearPred_R[1:4,]$year, yearPred_R[1:4,]$disabled, col="purple",type = "b", lty = 1,lwd=2)

legend("topright", legend=c("Predicted (Rural)", "Actual (Rural)","Predicted (Urban)", "Actual (Urban)"),
       col=c("red", "deeppink","blue","purple"), lty = 2:1, cex=0.8,lwd=2)


yearPred_D1=insertYear(1,2)
yearPred_D2=insertYear(2,2)
yearPred_D3=insertYear(3,2)

#D1 has null value, predict for null value
pred=0
for(i in 1:nrow(filter(dt2,dt2[,2] == 1))) {
  #predict for each row
  re=dt2[i,]$reg
  dv=dt2[i,]$develop
  house=dt2[i,]$cat
  
  #set year to predict
  t=2020
  
  #set total estimate
  d0=data.frame(reg=re,develop=dv,cat=house,Time.period=t,interval = "confidence")
  tpe=as.double(predict(model_tpe,d0))
  
  #set cthout
  d1=data.frame(reg=re,develop=dv,cat=house,Total.Point.estimate=tpe,Time.period=t,interval = "confidence")
  cthout=as.double(predict(model_cthout,d1))
  
  newdata=data.frame(reg=re,
                     develop=dv,
                     cat=house,
                     Total.Point.estimate=tpe,
                     Children.without.functional.difficulties.Point.estimate=cthout,
                     Time.period = t, interval = "confidence")
  
  pred=pred+as.double(predict(model,newdata))
}
meanPred=pred/nrow(filter(dt2,dt2[,2] == 1))

yearPred_D1[4,2]=meanPred

#D3 has no values, predict for all

# for(j in 1:(nrow(yearPred_D3)+y)){
#   pred=0
#   for(i in 1:nrow(dt2)) {
#     #predict for each row
#     re=dt2[i,]$reg
#     dv=3
#     house=dt2[i,]$cat
#     
#     #set year to predict
#     t=2016+j
#     
#     #set total estimate
#     d0=data.frame(reg=re,develop=dv,cat=house,Time.period=t,interval = "confidence")
#     tpe=as.double(predict(model_tpe,d0))
#     
#     #set cthout
#     d1=data.frame(reg=re,develop=dv,cat=house,Total.Point.estimate=tpe,Time.period=t,interval = "confidence")
#     cthout=as.double(predict(model_cthout,d1))
#     
#     newdata=data.frame(reg=re,
#                        develop=dv,
#                        cat=house,
#                        Total.Point.estimate=tpe,
#                        Children.without.functional.difficulties.Point.estimate=cthout,
#                        Time.period = t, interval = "confidence")
#     
#     pred=pred+as.double(predict(model,newdata))
#   }
#   meanPred=pred/nrow(dt2)
#   
#   if(j<=4)
#     yearPred_D3[j,2]=meanPred
#   else
#     yearPred_D3[j,]=data.frame(year=t,disabled=meanPred)
# }

yearPred_D1<-data.frame(future(yearPred_D1,5,1,2))
yearPred_D2<-data.frame(future(yearPred_D2,5,2,2))

plot(yearPred_D1$year, yearPred_D1$disabled, col="red",type = "b", lty = 2, asp = 0.05, lwd=2,
     main = "Percentage of children with disabilities attending to school group by Development Region", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred_D1[1:4,]$year, yearPred_D1[1:4,]$disabled, col="deeppink",type = "b", lty = 1,lwd=2)

lines(yearPred_D2$year, yearPred_D2$disabled, col="blue",type = "b", lty = 2,lwd=2)
lines(yearPred_D2[1:4,]$year, yearPred_D2[1:4,]$disabled, col="purple",type = "b", lty = 1,lwd=2)

# lines(yearPred_D3$year, yearPred_D3$disabled, col="green",type = "b", lty = 2,lwd=2)
# lines(yearPred_D3[1:4,]$year, yearPred_D3[1:4,]$disabled, col="dark green",type = "b", lty = 1,lwd=2)

legend("bottomright", legend=c("Predicted (Least Developt)", "Actual (Least Developt)","Predicted (Less Developt)", "Actual (Less Developt)"),
       col=c("red", "deeppink","blue","purple"), lty = 2:1, cex=0.8,lwd=2)
