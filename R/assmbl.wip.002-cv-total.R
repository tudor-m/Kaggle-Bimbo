
library(sgd)
library(penalized)
library(xgboost)
library(glmnet)
library(Ckmeans.1d.dp)
# DATA_SET = "CV-1"
# Cluster - defined by rows to be assembled:
#idx = c(1:10000) #e.g.

print(DATA_SET)
# PREDICT Demanda_uni_equil:

if (DATA_SET == "CV-1")
{
  fmla_c_xgb_1 =        c(  "B","AA","AB","Cw7","Cw6","Cw5","Cw4","Gw7","Gw6","Gw5","Gw4")#, #                 0.4781413  w. depth = 14, min_child_weight =5
  fmla_c_xgb_2 =        c("Dw7","Dw6","Dw5","Dw4","B","AA","AB","Cw7","Cw6","Cw5","Cw4","Hw7","Hw6","Hw5","Hw4")#, #  0.4773204, i.d.
  fmla_c_xgb_3 =        c(  "B","AA","AB","Jw7","Jw6","Jw5","Jw4","Hw7","Hw6","Hw5","Hw4")#, #                 0.4781413  w. depth = 14, min_child_weight =5
}

if (DATA_SET == "CV-2")
{
  fmla_c_xgb_1 =        c(  "B","AA","AB","Cw8","Cw7","Cw6","Cw5","Gw8","Gw7","Gw6","Gw5")#, #                 0.4781413  w. depth = 14, min_child_weight =5
  fmla_c_xgb_2 =        c("Dw8","Dw7","Dw6","Dw5","B","AA","AB","Cw8","Cw7","Cw6","Cw5","Hw8","Hw7","Hw6","Hw5")#, #  0.4773204, i.d.
  fmla_c_xgb_3 =        c(  "B","AA","AB","Jw8","Jw7","Jw6","Jw5","Hw8","Hw7","Hw6","Hw5")#, #                 0.4781413  w. depth = 14, min_child_weight =5
}

if (DATA_SET == "CV-TOTAL")
{
  fmla_c_xgb_1 =        c(  "B","AA","AB","Cw7","Cw6","Cw5","Cw4","Gw7","Gw6","Gw5","Gw4")#, #                 0.4781413  w. depth = 14, min_child_weight =5
  fmla_c_xgb_2 =        c("Dw7","Dw6","Dw5","Dw4","B","AA","AB","Cw7","Cw6","Cw5","Cw4","Hw7","Hw6","Hw5","Hw4")#, #  0.4773204, i.d.
  fmla_c_xgb_3 =        c(  "B","AA","AB","Jw7","Jw6","Jw5","Jw4","Hw7","Hw6","Hw5","Hw4")#, #                 0.4781413  w. depth = 14, min_child_weight =5
}

pred_test_xgb = list()
err_pred_test_xgb = list()

fmla_list = list(fmla_c_xgb_1,fmla_c_xgb_2,fmla_c_xgb_3)

for (jj in 1:length(fmla_list)) {
#for (jj in 1:1) {
# with XGBOOST:
print("XGB")
fmla_c = fmla_list[[jj]]
# sink(file="output.txt",split=TRUE)
for (fmla_c in list(fmla_c)) {
  
fmla_xgb = paste(fmla_c,collapse = "+")
# LOAD the train and test data:
df.train <- data.frame(id=idxTrain)
for (j in fmla_c)
  df.train[j] = getDataT(DATA_SET,paste("s_feat_train_all_",j,sep = ""))[idxTrain]
df.train$id <- NULL
df.train.target = getDataT(DATA_SET,"train")[idxTrain,]$Demanda_uni_equil
df.test <- data.frame(id=idxTest)
for (j in fmla_c)
  df.test[j] = getDataT(DATA_SET,paste("s_feat_test_all_",j,sep = ""))[idxTest]
df.test$id <- NULL
df.test.target = getDataT(DATA_SET,"test")[idxTest,]$Demanda_uni_equil
if (DATA_SET == "CV-1") {

# FIT on train ...
dtrain <- xgb.DMatrix(data = as.matrix(df.train), label=df.train.target, missing = NA)
dtest <- xgb.DMatrix(data = as.matrix(df.test), label=df.test.target, missing = NA)
watchlist <- list(train = dtrain, test = dtest)

log1pEval <- function(preds, dtrain)
{
  labels = getinfo(dtrain, "label")
  preds[which(preds<0)] = 0
  logs = (log1p(preds)-log1p(labels))^2
  err = as.numeric(sqrt(mean(logs,na.rm = TRUE)))
  return(list(metric="error",value=err))
}


for (min_child_w in 5:5) {
  for (max_d in 22) {
  print(c("max_d: ",max_d))
  print(c("fmla= ",fmla_c))
  print(c("min_child_weight: ",min_child_w))
  

nround = 70
param <- list(  
  #objective           = "multi:softprob", num_class = 4,
  objective           = "reg:linear",
  booster             = "gbtree",
  #booster             = "gblinear",
  base_score          = 0.5,
  eta                 = 0.025,#0.05, #0.02, # 0.06, #0.01,
  max_depth           = max_d, #changed from default of 8
  subsample           = 0.5, #0.9, # 0.7
  colsample_bytree    = 0.5, # 0.7
  #num_parallel_tree   = 2,
  nthread = 4,
  alpha = 0,    #0.0001,
  lambda = 0,
  gamma = 0,
  scale_pos_weight = 1,
  min_child_weight    = min_child_w, #4, #4
  eval_metric         = log1pEval,
  #eval_metric         = "rmse",
  early_stopping_rounds    = 2,
  maximize = FALSE
)

if (1==0) {
set.seed(100)
fit.cv.res = xgb.cv(param, dtrain,nrounds = nround,nfold = 5,metrics = "error",showsd = FALSE,prediction = TRUE)
}

set.seed(100)
fit.train = xgb.train(params=param,dtrain,nrounds=nround,print.every.n = 2,maximize = FALSE,watchlist)
if (1==0) {
xgb.plot.importance(xgb.importance(model=fit.train))
head(xgb.importance(model=fit.train))
}
saveDataT(fit.train,DATA_SET,paste(as.character(c("fit.train",".",jBin,".",jj)),collapse = ""))
# PREDICT on test ...
pred_test = predict(fit.train, as.matrix(df.test),missing = NA)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_xgb[[jj]] = pred_test
err_pred_test_xgb[[jj]] = err_pred_test
}
 
}
}
if (DATA_SET == "CV-2") {
fit.train = getDataT("CV-1",paste(as.character(c("fit.train",".",jBin,".",jj)),collapse = ""))
pred_test = predict(fit.train, as.matrix(df.test),missing = NA)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_xgb[[jj]] = pred_test
err_pred_test_xgb[[jj]] = err_pred_test

}
if (DATA_SET == "CV-TOTAL") {
  
  # FIT on train ...
  # random pick 10%:
  idxRand = sort(sample(1:nrow(df.train),round(0.01*nrow(df.train))))
  dtrain <- xgb.DMatrix(data = as.matrix(df.train[idxRand,]), label=df.train.target[idxRand], missing = NA)
  dtest <- xgb.DMatrix(data = as.matrix(df.test), label=df.test.target, missing = NA)
  watchlist <- list(train = dtrain, test = dtest)
  
  log1pEval <- function(preds, dtrain)
  {
    labels = getinfo(dtrain, "label")
    preds[which(preds<0)] = 0
    logs = (log1p(preds)-log1p(labels))^2
    err = as.numeric(sqrt(mean(logs,na.rm = TRUE)))
    return(list(metric="error",value=err))
  }
  
  
  for (min_child_w in 8:8) {  #5:5
    for (max_d in 40) { #22
      print(c("max_d: ",max_d))
      print(c("fmla= ",fmla_c))
      print(c("min_child_weight: ",min_child_w))
      
      
      nround = 50
      param <- list(  
        #objective           = "multi:softprob", num_class = 4,
        objective           = "reg:linear",
        booster             = "gbtree",
        #booster             = "gblinear",
        base_score          = 0.5,
        eta                 = 0.025,#0.05, #0.02, # 0.06, #0.01,
        max_depth           = max_d, #changed from default of 8
        subsample           = 0.5, #0.9, # 0.7
        colsample_bytree    = 0.5, # 0.7
        #num_parallel_tree   = 2,
        nthread = 2,
        alpha = 0,    #0.0001,
        lambda = 0,
        gamma = 0,
        scale_pos_weight = 1,
        min_child_weight    = min_child_w, #4, #4
        eval_metric         = log1pEval,
        #eval_metric         = "rmse",
        early_stopping_rounds    = 2,
        maximize = FALSE
      )
      
      if (1==0) {
        set.seed(100)
        fit.cv.res = xgb.cv(param, dtrain,nrounds = nround,nfold = 5,metrics = "error",showsd = FALSE,prediction = TRUE)
      }
      
      set.seed(100)
      fit.train = xgb.train(params=param,dtrain,nrounds=nround,print.every.n = 2,maximize = FALSE,watchlist)
      if (1==0) {
        xgb.plot.importance(xgb.importance(model=fit.train))
        head(xgb.importance(model=fit.train))
      }
      saveDataT(fit.train,DATA_SET,paste(as.character(c("fit.train",".",jBin,".",jj)),collapse = ""))
      # PREDICT on test ...
      pred_test = predict(fit.train, as.matrix(df.test),missing = NA)
      pred_test[which(pred_test<0)] = 0
      err_pred_test = errMeasure3(pred_test,df.test.target)
      if (VERBOSE == 1){
        print(err_pred_test)
      }
      pred_test_xgb[[jj]] = pred_test
      err_pred_test_xgb[[jj]] = err_pred_test
    }
    
  }
}

  

}}
#sink()
#######################################

#######################################
# Average the predictions:
s = 0; for (i in 1:length(pred_test_xgb)) s=s+pred_test_xgb[[i]]; s = s/3
pred_test_all_mean = s;
pred_test_all_mean.bak = pred_test_all_mean

# Fix the NA:
pred_test_all_mean[which(is.na(pred_test_all_mean))] = mean(pred_test_all_mean,na.omit = TRUE)
# Fix the <0:
pred_test_all_mean[which(pred_test_all_mean<0)] = 0

er3 = errMeasure3(pred_test_all_mean,df.test.target)
print(c("my xgb average pred:",er3))
#######################################

# PREDICT Demanda_uni_equil as per public script:

G_ = getDataT(DATA_SET,paste("s_feat_test_all_","G",sep = ""))[idxTest]
B_ = getDataT(DATA_SET,paste("s_feat_test_all_","B",sep = ""))[idxTest]
AA_ = getDataT(DATA_SET,paste("s_feat_test_all_","AA",sep = ""))[idxTest]
C_ = getDataT(DATA_SET,paste("s_feat_test_all_","C",sep = ""))[idxTest]
AB_ = getDataT(DATA_SET,paste("s_feat_test_all_","AB",sep = ""))[idxTest]

mean_pred_test_p = 0.718*G_ + 0.187*B_ + 0.115
mean_pred_test_p[which(is.na(mean_pred_test_p))] = 0.745*B_[which(is.na(mean_pred_test_p))] + 0.192
mean_pred_test_p[which(is.na(mean_pred_test_p))] = 0.822*AA_[which(is.na(mean_pred_test_p))] + 0.855
mean_pred_test_p[which(is.na(mean_pred_test_p))] = 0.53*C_[which(is.na(mean_pred_test_p))] + 0.95
mean_pred_test_p[which(is.na(mean_pred_test_p))] = 0.5*AB_[which(is.na(mean_pred_test_p))] + 1

err_mean_pred_test_p = errMeasure3(mean_pred_test_p,df.test.target)
print(c("public pred:",err_mean_pred_test_p))
sum(is.na(mean_pred_test_p))
#######################################

mean_pred_test_combined = 0.2*mean_pred_test_p+0.8*pred_test_all_mean
err_mean_combined = errMeasure3(mean_pred_test_combined,df.test.target)
print(c("combined pred:",err_mean_combined))
