deep_xgb <- function(trainX,trainY,validX,validY,seed0,objective0,eta0,md0,mc0,subs0,cols0,base0,round0,seed1=42,pw1,objective1,eta1,md1,mc1,subs1,cols1,base1,round1)
{
  sink("deep_xgb.out.txt")
  set.seed(seed0)
  param0 <- list(objective = objective0, 
                eta = eta0, 
                max_depth = md0,
                min_child_weight = mc0,
                subsample = subs0,
                colsample_bytree = cols0,
                base_score = base0,
                #eval_metric = mcc_eval,
                verbose = 2,
                print.every = 10L,
                maximize = FALSE)
  dtrain <- xgb.DMatrix(as.matrix(trainX), label = trainY)
  dvalid <- xgb.DMatrix(as.matrix(validX), label = validY)
  # generate model 0
  xgb_model0 <- xgb.train(data = dtrain, param0, nrounds = round0, watchlist = list(mod = dtrain, val = dvalid))
  pred_train_0 = predict(xgb_model0,as.matrix(trainX))
  pred_valid_0 = predict(xgb_model0,as.matrix(validX))

  #model the errors, 1:
  pw = pw1
  set.seed(seed1)
  param1 <- list(objective = objective1,
                 eta = eta1,
                 max_depth = md1,
                 min_child_weight = mc1,
                 subsample = subs1,
                 colsample_bytree = cols1,
                 base_score = base1,
                 #eval_metric = mcc_eval,
                 verbose = 2,
                 print.every = 10L,
                 maximize = FALSE)
  err_train_0 = trainY - pred_train_0
  err_valid_0 = validY - pred_valid_0
  dtrain1 <- xgb.DMatrix(as.matrix(trainX)^pw1, label = err_train_0)
  dvalid1 <- xgb.DMatrix(as.matrix(validX)^pw1, label = err_valid_0)      
  # generate model 1
  xgb_model1 <- xgb.train(data = dtrain1, param, nrounds = round1, watchlist = list(mod = dtrain1, val = dvalid1))
      
  pred_err_train_1 = predict(xgb_model1,as.matrix(trainX)^pw)
  pred_err_valid_1 = predict(xgb_model1,as.matrix(validX)^pw)

  ivec = vector();
  errvec = vector();
  for (i in seq(-3,3,0.1))
  {
    ivec = append(ivec,i);
    err = errMeasureRMSE(pred_train_0 + i*pred_err_train_1,trainY);
    errvec = append(errvec,err)
  }
  imin_train_1 = ivec[which.min(errvec)];
  imin_valid_1 = (mean(pred_valid_0)^pw)/(mean(pred_train_0)^pw)*imin_train_1
  for (i in seq(-3,3,0.1))
  {
    print(c(i,errMeasureRMSE(pred_valid_0+i*pred_err_valid_1,validY)))
  }
  pred_train_1 = pred_train_0 + imin_train_1*pred_err_train_1
  pred_valid_1 = pred_valid_0 + imin_valid_1*pred_err_valid_1
  imin_1 = imin_train_1
  sink()
  return(list(pred_train_0,pred_valid_0,pred_train_1,pred_valid_1,pw1,xgb_model0,pw1,xgb_model1,imin_1,mean(pred_train_0)))
}

deep_xgb.predict <- function(deep_xgb_ret,predictX)
{
  pred0         = predict(deep_xgb_ret[[6]],as.matrix(predictX))
  predict_err_1 = predict(deep_xgb_ret[[8]],as.matrix(predictX)^deep_xgb_ret[[7]])
  ret           = pred0 + deep_xgb_ret[[9]]*(mean(pred0)^deep_xgb_ret[[7]])/(deep_xgb_ret[[10]]^deep_xgb_ret[[7]])*predict_err_1
}