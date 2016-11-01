library("xgboost")
library("data.table")
nr1 = 1000
nr2 = 1000
dt1 = cbind(rnorm(nr1,1,0.8),rnorm(nr1,0,0.5),rnorm(nr1,2,0.83),rnorm(nr1,-1,0.1),rnorm(nr1,-5,1))
dt1 = as.data.table(dt1)
names(dt1) = c("a","b","c","d","e")
dt1$Response = dt1[[1]]+dt1[[2]]^2+sin(dt1[[3]])+atan(dt1[[4]])+log(abs(dt1[[5]]))

dt2 = cbind(rnorm(nr2,1,0.8),rnorm(nr2,0.,0.8),rnorm(nr2,2,0.83),rnorm(nr2,-1,0.1),rnorm(nr2,-5,0.8))
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

