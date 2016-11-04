deep_xgb <- function(seed0=42,trainX,trainY,validX,validY,objective0,eta0,md0,mc0,subs0,cols0,base0,round0,seed1=42,pw1,objective1,eta1,md1,mc1,subs1,cols1,base1,round1)
{
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
  dmodel1 <- xgb.DMatrix(as.matrix(trainX), label = trainY-pred_train_0)
  dvalid1 <- xgb.DMatrix(as.matrix(validX), label = validY-pred_valid_0)      
  # generate model 1
  xgb_model1 <- xgb.train(data = dmodel1, param, nrounds = round1, watchlist = list(mod = dtrain, val = dvalid))
      
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
  
  
  
    
}