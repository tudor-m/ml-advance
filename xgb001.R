library("xgboost")
library("data.table")

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


for (is in 100:100)
for (md in 26:26) for (mc in 6:6){
print(c(is,md,mc))
set.seed(is)
param <- list(objective = "reg:linear", 
              eta = 0.01, 
              max_depth = md,
              min_child_weight = mc,
              subsample = 0.95,
              colsample_bytree = 0.95,
              base_score = 0.50,
              #eval_metric = mcc_eval,
              verbose = 2,
              print.every = 10L,
              maximize = FALSE)

dmodel <- xgb.DMatrix(as.matrix(dt1[,-"Response",with=F]), label = dt1$Response)
dvalid <- xgb.DMatrix(as.matrix(dt2[,-"Response",with=F]), label = dt2$Response)

# generate model 
xgb_model0 <- xgb.train(data = dmodel, param, nrounds = 1000,
                       watchlist = list(mod = dmodel, val = dvalid))

pred10 = predict(xgb_model0,as.matrix(dt1[,-"Response",with=F]))
pred20 = predict(xgb_model0,as.matrix(dt2[,-"Response",with=F]))
#plot(pred10-dt1$Response)
#plot(pred20-dt2$Response)
print(errMeasureRMSE(pred10,dt1$Response))
print(errMeasureRMSE(pred20,dt2$Response))

}

#adjustment 1:
pw = 5
for (is in 100:100)
  for (md in 2:2) for (mc in 1:1){
    print(c(is,md,mc))
    set.seed(is)
    param <- list(objective = "reg:linear", 
                  eta = 0.01, 
                  max_depth = md,
                  min_child_weight = mc,
                  subsample = 0.95,
                  colsample_bytree = 0.95,
                  base_score = 0.,
                  #eval_metric = mcc_eval,
                  verbose = 2,
                  print.every = 10L,
                  maximize = FALSE)
    dmodel <- xgb.DMatrix(as.matrix(dt1[,-"Response",with=F])^pw, label = dt1$Response-pred10)
    dvalid <- xgb.DMatrix(as.matrix(dt2[,-"Response",with=F])^pw, label = dt2$Response-pred20)
    
    # generate model 
    xgb_model1 <- xgb.train(data = dmodel, param, nrounds = 1000,
                           watchlist = list(mod = dmodel, val = dvalid))
    
    pred11 = predict(xgb_model1,as.matrix(dt1[,-"Response",with=F])^pw)
    pred21 = predict(xgb_model1,as.matrix(dt2[,-"Response",with=F])^pw)
    #plot(pred10+pred11-dt1$Response)
    #plot(pred20+pred21-dt2$Response)
    ivec = vector();
    errvec = vector();
    for (i in seq(-3,3,0.1))
      {ivec = append(ivec,i);
      err = errMeasureRMSE(pred10+i*pred11,dt1$Response);
      errvec = append(errvec,err)
      #print(c(i,err))
      }
    imin1_1 = ivec[which.min(errvec)];
    imin2_1 = mean(pred20)^pw/mean(pred10)^pw*imin1_1
    #print(imin)
    for (i in seq(-3,3,0.1)){
      print(c(i,errMeasureRMSE(pred20+i*pred21,dt2$Response)))
    }
    plot(pred10-imin1_1*pred11-dt1$Response)
    plot(pred20-imin2_1*pred21-dt2$Response)
    print(errMeasureRMSE(pred10+imin1_1*pred11,dt1$Response))
    print(errMeasureRMSE(pred20+imin2_1*pred21,dt2$Response))
}

print(errMeasureRMSE(pred10,dt1$Response))
print(errMeasureRMSE(pred20,dt2$Response))
print(errMeasureRMSE(pred10+imin1_1*pred11,dt1$Response))
print(errMeasureRMSE(pred20+imin2_1*pred21,dt2$Response))

