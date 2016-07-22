
## R version of most popular local hotels
library(bit64)
library(lava)
library(data.table)
library(ggplot2)
library(caret)
library(plyr)
library(MASS)
library(glmnet)

source("futil.R")
#source("processSubmissions.R)")

s_fct_mean <- function(x)
{
  floor(mean(x))
}

s_fct_max <- function(x)
{
  max(x)
}
subName = ".wip.000.csv"


#######################################
#   Target-based learners:
#######################################

estRet = list()
estRetE = list()
s_feat_train_all = list()
s_feat_cv_all = list()
s_feat_test_all = list()
s_err_train_all = list()
s_err_cv_all = list()
s_err_test_all = list()
s_feat_list_all = list()


#######################################
class_name   = "AA"
s_feat_list = list("Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "AB"
s_feat_list = list("Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################


#######################################
class_name   = "A"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "B"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "C"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "D"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "E"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "F"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "G"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "H"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "I"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "J"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "K"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "A1"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "AA1"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "AB1"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################


#######################################
class_name   = "B1"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "C1"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "D1"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "E1"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "F1"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "G1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "H1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "I1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "J1"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "K1"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "A2"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "B2"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "C2"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "D2"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "E2"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "F2"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "G2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "H2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "I2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "J2"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "K2"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(c(class_name," error: ",s_err_test[[1]]))
#######################################

#######################################
class_name   = "AMAX"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "AAMAX"
s_feat_list = list("Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "ABMAX"
s_feat_list = list("Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################


#######################################
class_name   = "BMAX"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "CMAX"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "DMAX"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "EMAX"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "FMAX"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "GMAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "HMAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "IMAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################


#######################################
class_name   = "KMAX"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "A1MAX"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "AA1MAX"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "AB1MAX"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################


#######################################
class_name   = "B1MAX"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "C1MAX"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "D1MAX"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "E1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "F1MAX"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "G1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "H1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "I1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "J1MAX"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
class_name   = "K1MAX"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_cv <- merge(cv,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_cv$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_cv$V1))
s_feat_cv[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_cv = errMeasure(s_feat_cv$V1,cv$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_cv[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_cv_all[[class_name]] = s_feat_cv$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_cv_all[[class_name]] = s_err_cv[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test$V1))
s_feat_test[idxna2,]$V1 = bk
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
s_err_test_all[[class_name]] = s_err_test[[1]]
if (VERBOSE==1) print(s_err_test[[1]])
#######################################

#######################################
# list the "classes":
lbl = names(s_feat_list_all)
if (VERBOSE == 1)
{
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_train_all[[l]][1]))) # features and train errors
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_cv_all[[l]][1]))) # features and test errors
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test_all[[l]][1]))) # features and test errors
}

#######################################



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


# Venta_uni_hoy:
fmla_string4 = "B+C+D+E+B1+C1+D1+E1+G1"
fmla_string3 = "B+C+D1+G1"
fmla_string2 = "A+B+C+D+E+F+AB+I(A^2)+AMAX+BMAX+CMAX+DMAX+I(DMAX^0.5)+I(AMAX^2)+AB1+A1+B1+C1+B2+C2+D2+F2+G2+A1MAX+B1MAX+C1MAX+I(D1MAX^0.5)"
fmla_string1 = "AA1+AB1+A1+B1+C1+D1+E1+F1+G1+H1+I1+J1+K1+I(AA1MAX^2)+AB1MAX+A1MAX+B1MAX+C1MAX+D1MAX+E1MAX+F1MAX+G1MAX+H1MAX+I1MAX+J1MAX+K1MAX"
fmla_string = fmla_string4
fmla_glmnet = c("B","C","D","E","AA1","AB1","B1","C1","D1","E1","G1","B1MAX","C1MAX","D1MAX","E1MAX","B2","C2","D2","E2","G2")
fmla_glmnet = c("AA1","AB1","A1MAX","B1","C1","D1","E1","G1","B1MAX","C1MAX","D1MAX","E1MAX")
fmla_glmnet = c("AA","AB","AMAX","B","C","D","E","G","BMAX","CMAX","DMAX","EMAX")
fmla_glmnet = c("B1","C1","D1","E1","G1","B1MAX","C1MAX","D1MAX","E1MAX")
fmla_glmnet = c("B1","C1","D1","E1","G1")


# with GLMNET:
# fit on train ...
x = as.matrix(df.train[,fmla_glmnet])
y = as.matrix(train$Venta_uni_hoy)
fit.train <- glmnet(x, y, family="gaussian", alpha=0, lambda=NULL, nlambda=10)
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

# with GLM:
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



#######################################








