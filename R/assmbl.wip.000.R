##################
# Assembling:

df.train <- data.frame(id=train$id)
for (j in 1:length(s_feat_train_all))
  df.train[names(s_feat_train_all[j])] = s_feat_train_all[[j]]
df.train$id <- NULL

df.cv <- data.frame(id=cv$id)
for (j in 1:length(s_feat_cv_all))
  df.cv[names(s_feat_cv_all[j])] = s_feat_cv_all[[j]]
df.cv$id <- NULL

df.test <- data.frame(id=test$id)
for (j in 1:length(s_feat_test_all))
  df.test[names(s_feat_test_all[j])] = s_feat_test_all[[j]]
df.test$id <- NULL
#######################################


#######################################

# PREDICT Venta_uni_hoy:
fmla_string4 = "B+C+D+E+B1+C1+D1+E1+G1"
fmla_string3 = "B+C+D1+G1"
fmla_string2 = "A+B+C+D+E+F+AB+I(A^2)+AMAX+BMAX+CMAX+DMAX+I(DMAX^0.5)+I(AMAX^2)+AB1+A1+B1+C1+B2+C2+D2+F2+G2+A1MAX+B1MAX+C1MAX+I(D1MAX^0.5)"
fmla_string1 = "AA1+AB1+A1+B1+C1+D1+E1+F1+G1+H1+I1+J1+K1+I(AA1MAX^2)+AB1MAX+A1MAX+B1MAX+C1MAX+D1MAX+E1MAX+F1MAX+G1MAX+H1MAX+I1MAX+J1MAX+K1MAX"
fmla_string = fmla_string4

fmla_glmnet = c("B1","C1","D1","E1","G1")
#fmla_glmnet = c("AA1","A1","D1","E1","G1","H1","I1","K1")

fmla_c_penalized = c("B1","C1","D1","E1","G1");
fmla_c_penalized = c("AA1","A1","D1","E1","G1","H1","I1","K1");
# this is good as well: fmla_c_penalized = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","A1MAX","B1MAX","C1MAX","D1MAX","E1MAX","G1MAX");
fmla_c = fmla_c_penalized
fmla_penalized = as.formula(paste("train$Venta_uni_hoy ~ ",paste(fmla_c,collapse = "+")))

#fmla_c_xgb = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","A1MAX","B1MAX","C1MAX","D1MAX","E1MAX","G1MAX");
fmla_c_xgb = c("B1","C1","D1","E1","G1");
#fmla_c_xgb = c("AA1","A1","D1","E1","G1","H1","I1","K1");

fmla_c_sgd = c("B","C","D","E","B1","C1","D1","E1","G1")

# with GLMNET:
print("GLMNET")
# fit on train ...
x = as.matrix(df.train[,fmla_glmnet])
y = as.matrix(train$Venta_uni_hoy)
fit.lambda  = cv.glmnet(x,y)
fit.train <- glmnet(x, y, family="gaussian", alpha=0, lambda=fit.lambda$lambda.1se)
# predict on cv ...
x = as.matrix(df.cv[,fmla_glmnet])
pred_cv <- predict.glmnet(fit.train, x, type="link",s=fit.train$lambda[length(fit.train$lambda)])
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
x = as.matrix(df.test[,fmla_glmnet])
pred_test <- predict.glmnet(fit.train, x, type="link",s=fit.train$lambda[length(fit.train$lambda)])
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, test:")
  print(err_pred_test[[1]])
}
pred_cv_glmnet = pred_cv
pred_test_glmnet = pred_test
err_pred_cv_glmnet = err_pred_cv[[1]]
err_pred_test_glmnet = err_pred_test[[1]]


# with PENALIZED:
print("PENALIZED")
# fit on train ...
fmla = fmla_penalized
fit.train = penalized(fmla,data=df.train,model="linear",standardize = TRUE, lambda1 = 100, lambda2 = 100,trace=TRUE)
# predict on cv ...
pred_cv = predict(fit.train,df.cv[fmla_c_penalized])[,1]
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
pred_test = predict(fit.train,df.test[fmla_c_penalized])[,1]
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, cv:")
  print(err_pred_test[[1]])
}
pred_cv_penalized = pred_cv
pred_test_penalized = pred_test
err_pred_cv_penalized = err_pred_cv[[1]]
err_pred_test_penalized = err_pred_test[[1]]


# with GLM:
print("GLM")
# fit on train ...
fmla = as.formula(paste("train$Venta_uni_hoy ~ ",fmla_string,collapse = ""))
fit.train = glm(fmla,data=df.train,model=FALSE,family = gaussian(link=identity))
# predict on cv ...
pred_cv = predict.glm(fit.train,df.cv)
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
pred_test = predict.glm(fit.train,df.test)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, test:")
  print(err_pred_test[[1]])
}
pred_cv_glm = pred_cv
pred_test_glm = pred_test
err_pred_cv_glm = err_pred_cv[[1]]
err_pred_test_glm = err_pred_test[[1]]


# with SGD:
print("SGD")
# fit on train ...
fmla = as.formula(paste("train$Venta_uni_hoy ~ ",fmla_string,collapse = ""))
fit.train = sgd(fmla,data=df.train,model="lm")
# predict on cv ...
x = as.matrix(df.cv[fmla_c_sgd]);
x = cbind(1+0*x[,1],x)
pred_cv = predict(fit.train,x)
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
x = as.matrix(df.test[fmla_c_sgd]);
x = cbind(1+0*x[,1],x)
pred_test = predict(fit.train,x)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, test:")
  print(err_pred_test[[1]])
}
pred_cv_sgd = pred_cv
pred_test_sgd = pred_test
err_pred_cv_sgd = err_pred_cv[[1]]
err_pred_test_sgd = err_pred_test[[1]]


# with XGBOOST:
print("XGB")
# fit on train ...
dtrain <- xgb.DMatrix(data = as.matrix(df.train[fmla_c_xgb]), label=train$Venta_uni_hoy)
param <- list(  
  #objective           = "multi:softprob", num_class = 4,
  objective           = "reg:linear",
  booster             = "gbtree",
  #booster             = "gblinear",
  base_score          = 0,
  eta                 = 0.01, #0.02, # 0.06, #0.01,
  max_depth           = 4, #changed from default of 8
  subsample           = 0.7, #0.9, # 0.7
  colsample_bytree    = 0.7, # 0.7
  #num_parallel_tree   = 2,
  alpha = 0.001,    #0.0001,
  lambda = 0.05,
  min_child_weight    = 1
  
  # eval_metric         = RMSE
)
set.seed(100)
fit.train = xgb.train(params=param,dtrain,nrounds=400,print.every.n = 2,maximize = FALSE )
# predict on cv ...
pred_cv = predict(fit.train, as.matrix(df.cv[fmla_c_xgb]),missing = NaN)
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
pred_test = predict(fit.train, as.matrix(df.test[fmla_c_xgb]),missing = NaN)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Venta_uni_hoy)
if (VERBOSE == 1){
  print("Venta_uni_hoy, test:")
  print(err_pred_test[[1]])
}
pred_cv_xgb = pred_cv
pred_test_xgb = pred_test
err_pred_cv_xgb = err_pred_cv[[1]]
err_pred_test_xgb = err_pred_test[[1]]


# Average the predictions:
mean_pred_cv = (pred_cv_glm + pred_cv_glmnet + pred_cv_penalized + pred_cv_sgd + 1*pred_cv_xgb)/5
mean_pred_cv[which(is.na(mean_pred_cv))] = 1.2*s_feat_cv_all[["A1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["B1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["C1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["D1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["E1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["F1"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 1.2*s_feat_cv_all[["G1"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["J1"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 1.2*s_feat_cv_all[["AA1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["H1"]][which(is.na(mean_pred_cv))]
#mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["I1"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["K1"]][which(is.na(mean_pred_cv))]
err_mean_pred_cv = errMeasure(mean_pred_cv,cv$Venta_uni_hoy)
print(err_mean_pred_cv[[1]])
sum(is.na(mean_pred_cv))

mean_pred_test = (pred_test_glm + pred_test_glmnet + pred_test_penalized + pred_test_sgd + 0*pred_test_xgb)/4
mean_pred_test[which(is.na(mean_pred_test))] = 0.65*s_feat_test_all[["AA1"]][which(is.na(mean_pred_test))]
err_mean_pred_test = errMeasure(mean_pred_test,test$Venta_uni_hoy)
print(err_mean_pred_test[[1]])

#######################################

#######################################


# PREDICT Demanda_uni_equil:
fmla_string4 = "B+C+D+E+B1+C1+D1+E1+G1"
fmla_string3 = "B+C+D1+G1"
fmla_string2 = "A+B+C+D+E+F+AB+I(A^2)+AMAX+BMAX+CMAX+DMAX+I(DMAX^0.5)+I(AMAX^2)+AB1+A1+B1+C1+B2+C2+D2+F2+G2+A1MAX+B1MAX+C1MAX+I(D1MAX^0.5)"
fmla_string1 = "AA1+AB1+A1+B1+C1+D1+E1+F1+G1+H1+I1+J1+K1+I(AA1MAX^2)+AB1MAX+A1MAX+B1MAX+C1MAX+D1MAX+E1MAX+F1MAX+G1MAX+H1MAX+I1MAX+J1MAX+K1MAX"
fmla_string = fmla_string4

fmla_glmnet = c("B","C","D","E","G","BMAX","CMAX","DMAX","EMAX","GMAX")

fmla_c_penalized = c("B","C","D","E","G");
# this is good as well: fmla_c_penalized = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","A1MAX","B1MAX","C1MAX","D1MAX","E1MAX","G1MAX");
fmla_c = fmla_c_penalized
fmla_penalized = as.formula(paste("train$Demanda_uni_equil ~ ",paste(fmla_c,collapse = "+")))

#fmla_c_xgb = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","A1MAX","B1MAX","C1MAX","D1MAX","E1MAX","G1MAX");
fmla_c_xgb = c("B","C","D","E","G");

# with GLMNET:
print("GLMNET")
# fit on train ...
x = as.matrix(df.train[,fmla_glmnet])
y = as.matrix(train$Demanda_uni_equil)
fit.lambda  = cv.glmnet(x,y)
fit.train <- glmnet(x, y, family="gaussian", alpha=0, lambda=fit.lambda$lambda.1se)
# predict on cv ...
x = as.matrix(df.cv[,fmla_glmnet])
pred_cv <- predict.glmnet(fit.train, x, type="link",s=fit.train$lambda[length(fit.train$lambda)])
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
x = as.matrix(df.test[,fmla_glmnet])
pred_test <- predict.glmnet(fit.train, x, type="link",s=fit.train$lambda[length(fit.train$lambda)])
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, test:")
  print(err_pred_test[[1]])
}
pred_cv_glmnet = pred_cv
pred_test_glmnet = pred_test
err_pred_cv_glmnet = err_pred_cv[[1]]
err_pred_test_glmnet = err_pred_test[[1]]


# with PENALIZED:
print("PENALIZED")
# fit on train ...
fmla = fmla_penalized
fit.train = penalized(fmla,data=df.train,model="linear",standardize = TRUE, lambda1 = 100, lambda2 = 100,trace=TRUE)
# predict on cv ...
pred_cv = predict(fit.train,df.cv[fmla_c_penalized])[,1]
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
pred_test = predict(fit.train,df.test[fmla_c_penalized])[,1]
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, cv:")
  print(err_pred_test[[1]])
}
pred_cv_penalized = pred_cv
pred_test_penalized = pred_test
err_pred_cv_penalized = err_pred_cv[[1]]
err_pred_test_penalized = err_pred_test[[1]]


# with GLM:
print("GLM")
# fit on train ...
fmla = as.formula(paste("train$Demanda_uni_equil ~ ",fmla_string,collapse = ""))
fit.train = glm(fmla,data=df.train,model=FALSE,family = gaussian(link=identity))
# predict on cv ...
pred_cv = predict.glm(fit.train,df.cv)
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
pred_test = predict.glm(fit.train,df.test)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, test:")
  print(err_pred_test[[1]])
}
pred_cv_glm = pred_cv
pred_test_glm = pred_test
err_pred_cv_glm = err_pred_cv[[1]]
err_pred_test_glm = err_pred_test[[1]]


# with SGD:
print("SGD")
# fit on train ...
fmla = as.formula(paste("train$Demanda_uni_equil ~ ",fmla_string,collapse = ""))
fit.train = sgd(fmla,data=df.train,model="lm")
# predict on cv ...
x = as.matrix(df.cv[c("B","C","D","E","B1","C1","D1","E1","G1")]);
x = cbind(1+0*x[,1],x)
pred_cv = predict(fit.train,x)
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
x = as.matrix(df.test[c("B","C","D","E","B1","C1","D1","E1","G1")]);
x = cbind(1+0*x[,1],x)
pred_test = predict(fit.train,x)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, test:")
  print(err_pred_test[[1]])
}
pred_cv_sgd = pred_cv
pred_test_sgd = pred_test
err_pred_cv_sgd = err_pred_cv[[1]]
err_pred_test_sgd = err_pred_test[[1]]


# with XGBOOST:
print("XGB")
# fit on train ...
dtrain <- xgb.DMatrix(data = as.matrix(df.train[fmla_c_xgb]), label=train$Demanda_uni_equil)
param <- list(  
  #objective           = "multi:softprob", num_class = 4,
  objective           = "reg:linear",
  booster             = "gbtree",
  #booster             = "gblinear",
  base_score          = 0,
  eta                 = 0.01, #0.02, # 0.06, #0.01,
  max_depth           = 4, #changed from default of 8
  subsample           = 0.7, #0.9, # 0.7
  colsample_bytree    = 0.7, # 0.7
  #num_parallel_tree   = 2,
  alpha = 0.001,    #0.0001,
  lambda = 0.05,
  min_child_weight    = 1
  
  # eval_metric         = RMSE
)
set.seed(100)
fit.train = xgb.train(params=param,dtrain,nrounds=400,print.every.n = 2,maximize = FALSE )
# predict on cv ...
pred_cv = predict(fit.train, as.matrix(df.cv[fmla_c_xgb]),missing = NaN)
pred_cv[which(pred_cv<0)] = 0
err_pred_cv = errMeasure(pred_cv,cv$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, cv:")
  print(err_pred_cv[[1]])
}
# predict on test ...
pred_test = predict(fit.train, as.matrix(df.test[fmla_c_xgb]),missing = NaN)
pred_test[which(pred_test<0)] = 0
err_pred_test = errMeasure(pred_test,test$Demanda_uni_equil)
if (VERBOSE == 1){
  print("Demanda_uni_equil, test:")
  print(err_pred_test[[1]])
}
pred_cv_xgb = pred_cv
pred_test_xgb = pred_test
err_pred_cv_xgb = err_pred_cv[[1]]
err_pred_test_xgb = err_pred_test[[1]]


# Average the predictions:
mean_pred_cv = (pred_cv_glm + pred_cv_glmnet + pred_cv_penalized + pred_cv_sgd + pred_cv_xgb)/5
mean_pred_cv[which(is.na(mean_pred_cv))] = 0.85*s_feat_cv_all[["A"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 0.75*s_feat_cv_all[["G"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["AB"]][which(is.na(mean_pred_cv))]
mean_pred_cv[which(is.na(mean_pred_cv))] = 0.65*s_feat_cv_all[["AA"]][which(is.na(mean_pred_cv))]
err_mean_pred_cv = errMeasure(mean_pred_cv,cv$Demanda_uni_equil)
print(err_mean_pred_cv[[1]])

mean_pred_test = (pred_test_glm + pred_test_glmnet + pred_test_penalized + pred_test_sgd + pred_test_xgb)/5
mean_pred_test[which(is.na(mean_pred_test))] = 0.85*s_feat_test_all[["A"]][which(is.na(mean_pred_test))]
mean_pred_test[which(is.na(mean_pred_test))] = 0.75*s_feat_test_all[["G"]][which(is.na(mean_pred_test))]
mean_pred_test[which(is.na(mean_pred_test))] = 0.65*s_feat_test_all[["AB"]][which(is.na(mean_pred_test))]
mean_pred_test[which(is.na(mean_pred_test))] = 0.65*s_feat_test_all[["AA"]][which(is.na(mean_pred_test))]
err_mean_pred_test = errMeasure(mean_pred_test,test$Demanda_uni_equil)
print(err_mean_pred_test[[1]])

#######################################


#######################################



# PREDICT Demanda_uni_equil as per public script:

mean_pred_test = 0.718*s_feat_test_all[["G"]] + 0.187*s_feat_test_all[["B"]] + 0.115
mean_pred_test[which(is.na(mean_pred_test))] = 0.745*s_feat_test_all[["B"]][which(is.na(mean_pred_test))] + 0.192
mean_pred_test[which(is.na(mean_pred_test))] = 0.822*s_feat_test_all[["AA"]][which(is.na(mean_pred_test))] + 0.855
mean_pred_test[which(is.na(mean_pred_test))] = 0.53*s_feat_test_all[["C"]][which(is.na(mean_pred_test))] + 0.95
mean_pred_test[which(is.na(mean_pred_test))] = 0.5*s_feat_test_all[["AB"]][which(is.na(mean_pred_test))] + 1


err_mean_pred_test = errMeasure(mean_pred_test,test$Demanda_uni_equil)
print(err_mean_pred_test[[1]])
sum(is.na(mean_pred_test))

#######################################
