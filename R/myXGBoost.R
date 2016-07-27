library(xgboost)
#source("futil.R")

# (The "Error Function:")
RMSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  lab<-as.numeric(labels)
  preds<-as.numeric(preds)
  err <- sqrt(mean((preds-lab)^2))
  return(list(metric = "RMSE", value = err))
}

nr1 = nrow(inData1)
nr2 = nrow(inData2)
tra <- matrix(as.numeric(as.matrix(inData1,nrow=nr1)),nrow=nr1)     # train features
trg <- matrix(as.numeric(as.matrix(target1)))                       # train target
tes <- matrix(as.numeric(as.matrix(inData2,nrow=nr2)),nrow = nr2)   # test  features
nv = 4 # number of folds for cross-validation


pred_final = vector(mode="numeric",length=nrow(inData2))

hl = list()
tsize = nrow(inData1)
nsize = as.integer(nrow(tra)/nv)
set.seed(10)
hnv = sample(1:tsize,size=tsize) # reshufle randomly

pred_i = vector(mode="numeric",length=nrow(tes));
clf_all_ret = list()
pred_all_ret = list()
for (ii in 1:nv)
{
  set.seed(100)
  hl_ii <- hnv[((ii-1)*nsize+1):(ii*nsize)]

  dtrain <- xgb.DMatrix(data = as.matrix(tra[hl_ii,]), label=trg[hl_ii])
  dtest <- xgb.DMatrix(data = as.matrix(tra[-hl_ii,]), label=trg[-hl_ii])
  watchlist<-list(train = dtrain, test = dtest)
  param <- list(  
    #objective           = "multi:softprob", num_class = 4,
    objective           = "reg:linear",
    #booster             = "gbtree",
    booster             = "gblinear",
    base_score          = 0,
    eta                 = 0.5, #0.02, # 0.06, #0.01,
    max_depth           = 16, #changed from default of 8
    subsample           = 0.5, #0.9, # 0.7
    colsample_bytree    = 0.7, # 0.7
    #num_parallel_tree   = 2,
    alpha = 0.01,    #0.0001,
    lambda = 0.05,
    min_child_weight    = 1
    # eval_metric         = RMSE
  )
  clf <- xgb.train(
    params              = param,
    data                = dtrain, 
    nrounds             = 5000, #300, #280, #125, #250, # changed from 300
    verbose             = 0,
    print.every.n       = 200,
    early.stop.round    = 2000,
    watchlist           = watchlist,
    maximize            = FALSE,
    feval               = RMSE
  )
  print(c(ii,"best.score:",clf$bestScore))
  # print(xgb.importance(model=clf))
  write(c("best.score:",clf$bestScore),file = "wip.log",append=TRUE)
  clf_all_ret[[ii]] = clf
  pred_all_ret[[ii]] = predict(clf, data.matrix(tes))
}
pred_ret = 


#write.table(predFinal,file="submission009.csv",sep=",",row.names=FALSE,col.names=c("Id","Sales"))

