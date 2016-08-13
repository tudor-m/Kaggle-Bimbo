
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

fmla_c_glmnet =     c("A","AA","AB","B","C","D","E","G","ABMAX","BMAX","CMAX","DMAX","EMAX","GMAX")
fmla_c_glmnet =     c("AA","AB","F","G")
fmla_c_glmnet =     c("AA","AB","F","G","AAw7","ABw7","Fw7","Gw7")
fmla_c_penalized =  c("A","AA","AB","B","C","D","E","G","ABMAX","BMAX","CMAX","DMAX","EMAX","GMAX")
fmla_c_penalized =     c("AA","AB","F","G")
#fmla_c_xgb =        c("A","AA","AB","B","C","D","E","G","ABMAX","BMAX","CMAX","DMAX","EMAX","GMAX","F","H","I","J","K")
fmla_c_xgb =        c("B","AA","AB","Aw7","AAw7","ABw7","Aw6","AAw6","ABw6","Aw5","AAw5","ABw5") # good, 0.49, depth=10
fmla_c_xgb =        c("NAw6","NBw6","NCw6","Aw7","Aw6","Aw5","Aw4","B","AA","AB","Aw7","AAw7","ABw7","Aw6")#, # GOOD 0.487 depth 12
fmla_c_xgb =        c("NAw6","NBw6","NCw6","Aw7","Aw6","Aw5","Aw4","B","AA","AB","AAw7","ABw7")#, # GOOD 0.4869 depth 14
fmla_c_xgb =        c("NAw6","NBw6","NCw6","Aw7","Aw6","Aw5","Aw4","B","AA","AB","AAw7","AAw6","ABw7","ABw6")#, # 


fmla_c_xgb =        c("B","Bw7","Bw6","Bw5","Bw4","Fw7","Gw7","Fw6","Gw6","Fw5","Gw5","Fw4","Gw4",
                      "Aw7","AAw7","ABw7",
                      "ACw7","ADw7","AW6","AAw6","ABw6","ACw6","ADw6","Aw5","AAw5","ABw5","ACw5","ADw5","AAw4","ABw4","ACw4","ADw4","AAw3","ABw3","ACw3","ADw3",
                      "NAw7","NBw7","NCw7","NDw7",
                      "NAw6","NBw6","NCw6","NDw6",
                      "NAw5","NBw5","NCw5","NDw5",
                      "NAw4","NBw4","NCw4","NDw4",
                      "NAw3","NBw3","NCw3","NDw3")

fmla_c_xgb =        c(
                      "NAw6","NCw6",#"NDw6",
                      "Aw7","Aw6","Aw5","Aw4",
                      "Gw7","Gw6","Gw5","Gw4",
                      "B","AA")#,"AB","AAw7","ABw7","AAw6","ABw6")
                        #,"AAw6","ABw6","Aw5","AAw5","ABw5"
                      )#,#"Aw3",
                      "NAw7","NBw7","NCw7","NDw7",
                      "NAw5","NBw5","NCw5","NDw5",
                      "NAw4","NBw4","NCw4","NDw4",
                      "NAw3","NBw3","NCw3","NDw3"
                      )


  

fmla_c_xgb =        c(
  "Aw7","Aw6","Aw5","Aw4")#"Aw3")


fmla_c_xgb =        c("A","Aw7","Aw6","Aw5","Aw4")
fmla_c_sgd =        c("A","AA","AB","B","C","D","E","G","ABMAX","BMAX","CMAX","DMAX","EMAX","GMAX")
fmla_c_sgd =     c("AA","AB","F","G")
fmla_c_glm =        c("A","AA","AB","B","C","D","E","G","ABMAX","BMAX","CMAX","DMAX","EMAX","GMAX")
fmla_c_glm =     c("AA","AB","F","G")

# with GLMNET:
print("GLMNET")
fmla_c = fmla_c_glmnet
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
# FIT on train ...
x = as.matrix(df.train)
y = as.matrix(df.train.target)
fit.lambda  = cv.glmnet(x,y)
fit.train <- glmnet(x, y, family="gaussian", alpha=0, lambda=fit.lambda$lambda.1se)
# PREDICT on test cv ...
x = as.matrix(df.test)
pred_test <- predict.glmnet(fit.train, x, type="link",s=fit.train$lambda[length(fit.train$lambda)])
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_glmnet = pred_test
err_pred_test_glmnet = err_pred_test


# with PENALIZED:
print("PENALIZED")
fmla_c = fmla_c_penalized
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
# FIT on train ...
fmla_penalized = as.formula(paste("df.train.target ~ ",paste(names(df.train),collapse = "+")))
fmla = fmla_penalized
fit.train = penalized(fmla,data=df.train,model="linear",standardize = TRUE, lambda1 = 2, lambda2 = 4,trace=FALSE,epsilon = 1e-7)
# PREDICT on test cv ...
pred_test = predict(fit.train,df.test)[,1]
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_penalized = pred_test
err_pred_test_penalized = err_pred_test


# with GLM:
print("GLM")
fmla_c = fmla_c_glm
fmla_glm = paste(fmla_c,collapse = "+")
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
# FIT on train ...
fmla = as.formula(paste("df.train.target ~ ",fmla_glm,collapse = ""))
fit.train = glm(fmla,data=df.train,model=FALSE,family = gaussian(link=identity))
# PREDICT on test ...
pred_test = predict.glm(fit.train,df.test)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_glm = pred_test
err_pred_test_glm = err_pred_test


# with SGD:
print("SGD")
fmla_c = fmla_c_sgd
fmla_sgd = paste(fmla_c,collapse = "+")
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
# FIT on train ...
fmla = as.formula(paste("df.train.target ~ ",fmla_sgd,collapse = ""))
fit.train = sgd(fmla,data=df.train,model="glm",model.control=list(L1=0, L2=0))
# PREDICT on test cv ...
x = as.matrix(df.test);
x = cbind(1+0*x[,1],x)
pred_test = predict(fit.train,x)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_sgd = pred_test
err_pred_test_sgd = err_pred_test


# with XGBOOST:
print("XGB")
fmla_c = fmla_c_xgb
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

nround = 40
param <- list(  
  #objective           = "multi:softprob", num_class = 4,
  objective           = "reg:linear",
  booster             = "gbtree",
  #booster             = "gblinear",
  base_score          = 0.5,
  eta                 = 0.05,#0.05, #0.02, # 0.06, #0.01,
  max_depth           = 14, #changed from default of 8
  subsample           = 0.5, #0.9, # 0.7
  colsample_bytree    = 0.5, # 0.7
  #num_parallel_tree   = 2,
  nthread = 4,
  alpha = 0,    #0.0001,
  lambda = 0,
  gamma = 0,
  scale_pos_weight = 1,
  min_child_weight    = 4, #2
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
xgb.plot.importance(xgb.importance(model=fit.train))
head(xgb.importance(model=fit.train))
# PREDICT on test ...
pred_test = predict(fit.train, as.matrix(df.test),missing = NA)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure3(pred_test,df.test.target)
if (VERBOSE == 1){
  print(err_pred_test)
}
pred_test_xgb = pred_test
err_pred_test_xgb = err_pred_test


#######################################


# Average the predictions:
err_pred_test_all = c(err_pred_test_glm,err_pred_test_glmnet,err_pred_test_penalized,err_pred_test_sgd,err_pred_test_xgb)
pred_test_all = cbind(pred_test_glm,pred_test_glmnet,pred_test_penalized,pred_test_sgd,pred_test_xgb)

# Fix the NAs:
pred_test_all.bak = pred_test_all

pred_test_all = pred_test_all.bak
for (j in 1:4)
{
  idx_na = which(is.na(pred_test_all[,j]))
  pred_test_all[idx_na,j] = 1.5*pred_test_all[idx_na,5]
}

print(err_pred_test_all)
coef_pred_test_all = c(1,1,1,1,0.5)
mean_pred_test = rowSums(t(coef_pred_test_all*t(pred_test_all)))/sum(coef_pred_test_all)
er3 = errMeasure3(mean_pred_test,df.test.target)
print(c("my average pred:",er3))

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

mean_pred_test_combined = 0.5*mean_pred_test_p+0.5*mean_pred_test
err_mean_combined = errMeasure3(mean_pred_test_combined,df.test.target)
print(c("combined pred:",err_mean_combined))
