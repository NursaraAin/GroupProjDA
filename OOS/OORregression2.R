library(psych)
library(caret)
library(dplyr)
library(car)

oor=read.csv("HousingOOR.csv")
dt1=subset(oor,select = c(reg,develop,level,X,
                          Total.Point.estimate,Children.without.functional.difficulties.Point.estimate,
                          Children.with.functional.difficulties.Point.estimate,Time.period))
corPlot(cor(dt1))

set.seed(8)
split1<- sample(c(rep(0, 0.7 * nrow(dt1)), rep(1, 0.3 * nrow(dt1))))
table(split1)
train=dt1[split1==0,]
test=dt1[split1==1,]

model=lm(Children.with.functional.difficulties.Point.estimate~.,data = train)
dtPred=predict(model,test)

rmse(test$Children.with.functional.difficulties.Point.estimate, dtPred)
#2.270764

#model=lm(Children.with.functional.difficulties.Point.estimate~.,data = dt1)

summary(model)
# Call:
#   lm(formula = Children.with.functional.difficulties.Point.estimate ~ 
#        ., data = train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.2412 -1.7173 -0.6061  0.9210 10.6210 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                             -536.2213   720.2423  -0.745    0.459    
# reg                                                       -0.2311     0.2999  -0.771    0.443    
# develop                                                   -0.6709     0.7290  -0.920    0.360    
# level                                                      0.4489     0.4004   1.121    0.266    
# X                                                          0.5994     0.6604   0.908    0.367    
# Total.Point.estimate                                       4.7735     0.3821  12.494  < 2e-16 ***
#   Children.without.functional.difficulties.Point.estimate   -3.8013     0.3869  -9.824  4.1e-15 ***
#   Time.period                                                0.2665     0.3566   0.747    0.457    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.707 on 75 degrees of freedom
# Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9763 
# F-statistic: 483.1 on 7 and 75 DF,  p-value: < 2.2e-16


plot(model,2)
#require regression for tpe
dt1_tpe=subset(train,select = -c(Children.without.functional.difficulties.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_tpe=lm(Total.Point.estimate~.,data = dt1_tpe)
summary(model_tpe)
# lm(formula = Total.Point.estimate ~ ., data = dt1_tpe)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.256  -9.286  -2.427   6.926  42.921 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -235.8479  3791.3357  -0.062  0.95056   
# reg            4.7595     1.4932   3.187  0.00207 **
#   develop       -0.8227     3.7722  -0.218  0.82794   
# level          3.8127     2.0324   1.876  0.06445 . 
# X            -10.9006     3.1967  -3.410  0.00104 **
#   Time.period    0.1216     1.8773   0.065  0.94851   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 14.35 on 77 degrees of freedom
# Multiple R-squared:  0.3029,	Adjusted R-squared:  0.2576 
# F-statistic: 6.692 on 5 and 77 DF,  p-value: 3.173e-05

plot(model_tpe,2)

#require regression for cwithout
dt1_cthout=subset(train,select = -c(Total.Point.estimate,Children.with.functional.difficulties.Point.estimate))
model_cthout=lm(Children.without.functional.difficulties.Point.estimate~.,data=dt1_cthout)
summary(model_cthout)
# call:
#   lm(formula = Children.without.functional.difficulties.Point.estimate ~ 
#        ., data = dt1_cthout)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.208  -8.718  -3.010   7.631  42.289 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.047e+01  3.744e+03  -0.003 0.997775    
# reg          4.726e+00  1.474e+00   3.205 0.001965 ** 
#   develop     -4.025e-01  3.725e+00  -0.108 0.914226    
# level        3.547e+00  2.007e+00   1.768 0.081088 .  
# X           -1.109e+01  3.156e+00  -3.514 0.000743 ***
#   Time.period  9.757e-03  1.854e+00   0.005 0.995814    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 14.17 on 77 degrees of freedom
# Multiple R-squared:  0.302,	Adjusted R-squared:  0.2566 
# F-statistic: 6.662 on 5 and 77 DF,  p-value: 3.329e-05
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
    re=dt1[i,]$reg
    dv=dt1[i,]$develop
    indi=dt1[i,]$level
    house=dt1[i,]$X
    
    #set year to predict
    t=yearPred1[nrow(yearPred1),]$year+1
    
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
  meanPred=pred/nrow(dt1)
  #create data frame
  newPred=data.frame(
    year=yearPred1[nrow(yearPred1),]$year+1,
    anarDisabled=meanPred
  )
  #insert to yearPred1
  yearPred1[nrow(yearPred1) + 1,]=newPred
}


plot(yearPred1$year, yearPred1$oorDisabled, col="red",type = "b", lty = 2, frame=FALSE,
     main = "Percentage of children with disabilities dropping out from school", xlab="Year",ylab="Percentage of children with disabilities")
lines(yearPred1[1:4,]$year, yearPred1[1:4,]$oorDisabled, col="blue",type = "b", lty = 1)
legend("bottomright", legend=c("Predicted", "Actual"),
       col=c("red", "blue"), lty = 2:1, cex=0.8)
