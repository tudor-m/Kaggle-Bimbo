
## R version of most popular local hotels
library(bit64)
library(lava)
library(data.table)
library(ggplot2)
library(caret)
library(rmongodb)

source("futil.R")
#source("processSubmissions.R)")

s_fct_mean <- function(x)
{
  floor(mean(x))
}

subName = ".wip.000.csv"


#######################################
#   Target-based learners:
#######################################

estRet = list()
estRetE = list()
s_feat_train_all = list()
s_feat_test_all = list()
s_feat_test2_all = list()
s_err_train_all = list()
s_err_test_all = list()
s_err_test2_all = list()
s_feat_list_all = list()

#######################################
class_name   = "A"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "B"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "C"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "D"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "E"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "F"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "G"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "H"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "I"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "J"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "K"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "A1"
s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "B1"
s_feat_list = list("Ruta_SAK","Producto_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "C1"
s_feat_list = list("Agencia_ID","Producto_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "D1"
s_feat_list = list("Ruta_SAK","Cliente_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "E1"
s_feat_list = list("Agencia_ID","Cliente_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "F1"
s_feat_list = list("Agencia_ID","Ruta_SAK","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "G1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "H1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "I1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "J1"
s_feat_list = list("Producto_ID","Canal_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "K1"
s_feat_list = list("Cliente_ID","Canal_ID","Venta_uni_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "A2"
s_feat_list = list("Cliente_ID","Producto_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "B2"
s_feat_list = list("Ruta_SAK","Producto_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "C2"
s_feat_list = list("Agencia_ID","Producto_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "D2"
s_feat_list = list("Ruta_SAK","Cliente_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "E2"
s_feat_list = list("Agencia_ID","Cliente_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "F2"
s_feat_list = list("Agencia_ID","Ruta_SAK","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "G2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "H2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "I2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "J2"
s_feat_list = list("Producto_ID","Canal_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}

s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(s_err_test2[[1]])
#######################################

#######################################
class_name   = "K2"
s_feat_list = list("Cliente_ID","Canal_ID","Venta_hoy")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]

bk = 0.4*mean(s_feat_test$V1,na.rm = TRUE)
idxna = which(is.na(s_feat_test$V1))
s_feat_test[idxna,]$V1 = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

if (VERBOSE == 1)
{
  print(s_err_train[[1]])
  print(s_err_test[[1]])
}


s_feat_train_all[[class_name]] = s_feat_train$V1
s_feat_test_all[[class_name]] = s_feat_test$V1
s_err_train_all[[class_name]] = s_err_train[[1]]
s_err_test_all[[class_name]] = s_err_test[[1]]
s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_test2 <- merge(test2,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
idxna2 = which(is.na(s_feat_test2$V1))
s_feat_test2[idxna2,]$V1 = bk
s_feat_test2_all[[class_name]] = s_feat_test2$V1
s_err_test2 = errMeasure(s_feat_test2$V1,test2$Demanda_uni_equil)
s_err_test2_all[[class_name]] = s_err_test2[[1]]
if (VERBOSE==1) print(c(class_name," error: ",s_err_test2[[1]]))
#######################################




#######################################
# list the "classes":
lbl = names(s_feat_list_all)
if (VERBOSE == 1)
{
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_train_all[[l]][1]))) # features and train errors
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test_all[[l]][1]))) # features and test errors
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test2_all[[l]][1]))) # features and test errors
}

#######################################




##################
# Assembling:

df.test <- data.frame(id=test$id)
for (j in 1:length(s_feat_test_all))
  df.test[names(s_feat_test_all[j])] = s_feat_test_all[[j]]
df.test$id <- NULL

# try glm:
fit = glm(test$Demanda_uni_equil ~ .,data=df.test)

df.test2 <- data.frame(id=test2$id)
for (j in 1:length(s_feat_test2_all))
  df.test2[names(s_feat_test2_all[j])] = s_feat_test2_all[[j]]
df.test2$id <- NULL

pred2 = predict.glm(fit,df.test2)
pred2[which(pred2<0)] = 0
errPred2 = errMeasure(pred2,test2$Demanda_uni_equil)
print(c("error: ",errPred2[[1]]))

#######################################








