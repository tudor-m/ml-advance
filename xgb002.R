library("xgboost")
library("data.table")
source("deep_xgb.R")

errMeasureRMSE <- function(v1,v2)
{
  sqrt(mean((v1 - v2)^2))
}

nr1 = 1000
nr2 = 1000
dt1 = cbind(rnorm(nr1,1,0.2),rnorm(nr1,0,0.5),rnorm(nr1,2,0.83),rnorm(nr1,-1,0.1),rnorm(nr1,-5,1))
dt1 = as.data.table(dt1)
names(dt1) = c("a","b","c","d","e")
dt1$Response = dt1[[1]]+dt1[[2]]^2+sin(dt1[[3]])+atan(dt1[[4]])+log(abs(dt1[[5]]))

dt2 = cbind(rnorm(nr2,1,0.8),rnorm(nr2,0.,1),rnorm(nr2,2,0.25),rnorm(nr2,-1,0.5),rnorm(nr2,-5,0.5))
dt2 = as.data.table(dt2)
names(dt2) = c("a","b","c","d","e")
dt2$Response = dt2[[1]]+dt2[[2]]^2+sin(dt2[[3]])+atan(dt2[[4]])+log(abs(dt2[[5]]))

for (md in 26:26) for (mc in 6:6)
{
  print(c(is,md,mc))
  set.seed(is)
  #deep_xgb <- function(trainX,trainY,validX,validY,seed0,objective0,eta0,md0,mc0,subs0,cols0,base0,round0,seed1,pw1,objective1,eta1,md1,mc1,subs1,cols1,base1,round1)
    
  trainX = dt1[,-"Response",with=F]
  trainY = dt1$Response
  validX = dt2[,-"Response",with=F]
  validY = dt2$Response
  seed0 = 100
  objective0 = "reg:linear"
  eta0 = 0.01
  md0 = 26
  mc0 = 6
  subs0 = 0.95
  cols0 = 0.95
  base0 = 0.50
  round0 = 1000
  pw1 = 5
  seed1 = 100
  objective1 = "reg:linear"
  eta1 = 0.01
  md1 = 2
  mc1 = 1
  subs1 = 0.95
  cols1 = 0.95
  base1 = 0.0
  round1 = 1000

  ret = deep_xgb(trainX,trainY,validX,validY,seed0,objective0,eta0,md0,mc0,subs0,cols0,base0,round0,seed1,pw1,objective1,eta1,md1,mc1,subs1,cols1,base1,round1)

}


print(errMeasureRMSE(ret[[1]],dt1$Response))
print(errMeasureRMSE(ret[[2]],dt2$Response))
print(errMeasureRMSE(ret[[3]],dt1$Response))
print(errMeasureRMSE(ret[[4]],dt2$Response))

pred_test = deep_xgb.predict(ret,validX)
sum(ret[[4]]-pred_test)

