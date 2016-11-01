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

dt2 = cbind(rnorm(nr2,1,0.8),rnorm(nr2,0.,0.8),rnorm(nr2,2,0.5),rnorm(nr2,-1,0.5),rnorm(nr2,-5,0.8))
dt2 = as.data.table(dt2)
names(dt2) = c("a","b","c","d","e")
dt2$Response = dt2[[1]]+dt2[[2]]^2+sin(dt2[[3]])+atan(dt2[[4]])+log(abs(dt2[[5]]))


for (is in 100:100)
for (md in 12:12) for (mc in 4:4){
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
xgb_model <- xgb.train(data = dmodel, param, nrounds = 65,
                       watchlist = list(mod = dmodel, val = dvalid))

pred1 = predict(xgb_model,as.matrix(dt1[,-"Response",with=F]))
pred2 = predict(xgb_model,as.matrix(dt2[,-"Response",with=F]))
plot(pred1-dt1$Response)
plot(pred2-dt2$Response)
}

# first adjustment:
for (is in 100:100)
  for (md in 22:22) for (mc in 4:4){
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
    pw = 1
    dmodel <- xgb.DMatrix(as.matrix(dt1[,-"Response",with=F])^pw, label = dt1$Response-pred1)
    dvalid <- xgb.DMatrix(as.matrix(dt2[,-"Response",with=F])^pw, label = dt2$Response-pred2)
    
    # generate model 
    xgb_model1 <- xgb.train(data = dmodel, param, nrounds = 65,
                           watchlist = list(mod = dmodel, val = dvalid))
    
    pred11 = predict(xgb_model1,as.matrix(dt1[,-"Response",with=F])^pw)
    pred21 = predict(xgb_model1,as.matrix(dt2[,-"Response",with=F])^pw)
    plot(pred1+pred11-dt1$Response)
    plot(pred2+pred21-dt2$Response)
    ivec = vector();
    errvec = vector();
    for (i in seq(-2,2,0.1))
      {ivec = append(ivec,i);
      err = errMeasureRMSE(pred1+i*pred11,dt1$Response);
      errvec = append(errvec,err)
      print(c(i,err))
      }
    imin = ivec[which.min(errvec)];
    print(imin)
    for (i in seq(-2,2,0.1)) print(c(i,errMeasureRMSE(pred2+i*pred21,dt2$Response)))
    plot(pred1+imin*pred11-dt1$Response)
    plot(pred2+imin*pred21-dt2$Response)
    print(errMeasureRMSE(pred1+imin*pred11,dt1$Response))
    print(errMeasureRMSE(pred2+imin*pred21,dt2$Response))
    
  }

