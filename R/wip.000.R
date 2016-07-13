## R version of most popular local hotels
library(bit64)
library(lava)
library(data.table)
library(ggplot2)
library(caret)
library(rmongodb)

source("futil.R")
#source("processSubmissions.R)")

# validation:
# 0 - TEST
# 1 - CV
# 2 - CV with a very small set
VALIDATION = 2
#VALIDATION = 1

train <- 
  fread('../data/train.csv', header=TRUE,
        select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))

if (VALIDATION == 1) # Full CV (cross-validation)
{
  #nCli = 10000;
  #set.seed(2300)
  trainCli = unique(train$Cliente_ID) # get all the clients
  trainWeeks = c(3,4,5,6,7)
  testWeeks = c(8)
  testWeeks2 = c(9)
  idxTrain = which(train$Cliente_ID %in% trainCli & train$Semana %in% trainWeeks)
  idxTest = which(train$Cliente_ID %in% trainCli & train$Semana %in% testWeeks)
  idxTest2 = which(train$Cliente_ID %in% trainCli & train$Semana %in% testWeeks2)
  test = train[idxTest,]
  test2 = train[idxTest2,]
  train = train[idxTrain,]
  train$id = 1:nrow(train)
  test$id = 1:nrow(test)
  test2$id = 1:nrow(test2)
  remove(idxTrain)
  remove(idxTest)
  remove(idxTest2)
  remove(trainCli)
}

if (VALIDATION == 2) # short set of train/test for quick CV
{
  set.seed(2300)

  nCli = 50000;
  jBin = 8;
  trainCli = sample(unique(train$Cliente_ID),nCli*(1+jBin))[((jBin-1)*nCli+1):(jBin*nCli)]
  trainWeeks = c(3,4,5,6,7)
  testWeeks = c(8)
  testWeeks2 = c(9)
  idxTrain = which(train$Cliente_ID %in% trainCli & train$Semana %in% trainWeeks)
  idxTest = which(train$Cliente_ID %in% trainCli & train$Semana %in% testWeeks)
  idxTest2 = which(train$Cliente_ID %in% trainCli & train$Semana %in% testWeeks2)
  test = train[idxTest,]
  test2 = train[idxTest2,]
  train = train[idxTrain,]
  train$id = 1:nrow(train)
  test$id = 1:nrow(test)
  test2$id = 1:nrow(test2)
}

if (VALIDATION == 0)
{
  test <- 
    fread('../data/test.csv', header=TRUE,
          select = c("row_id","x","y","accuracy","time"))
}


top_k <- function(hc,v1,k){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(k,length(hc_sorted))
  # n <- min(3,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=",")
}

top_kk <- function(plid,n,k){
  tt = plid[order(n,decreasing = TRUE)]
  nn = min(k,length(tt))
  paste( tt[1:nn],collapse=",")
}

top_score_kk <- function(plid,n,k){
  tt = n[order(n,decreasing = TRUE)]
  nn = min(k,length(tt))
  paste( tt[1:nn],collapse=",")
}

top_prc_kk <- function(plid,n,k){
  tt = n[order(n,decreasing = TRUE)]/sum(n)
  nn = min(k,length(tt))
  paste( tt[1:nn],collapse=",")
}

top_bprc_kk <- function(plid,n,k,bp){
  tt = n[order(n,decreasing = TRUE)]/sum(n)*bp
  nn = min(k,length(tt))
  paste( tt[1:nn],collapse=",")
}

top_val_k <- function(hc,v1,k){
  hc_sorted <- v1[order(v1,decreasing=TRUE)]
  n <- min(k,length(hc_sorted))
  # n <- min(3,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=",")
}

top_prc_k <- function(hc,v1,k){
  hc_sorted <- v1[order(v1,decreasing=TRUE)]
  hc_sorted <- hc_sorted/sum(hc_sorted)
  n <- min(k,length(hc_sorted))
  # n <- min(3,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=",")
}

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

print(s_err_train[[1]])
print(s_err_test[[1]])

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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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
print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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
print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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
print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
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

print(s_err_train[[1]])
print(s_err_test[[1]])


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
print(s_err_test2[[1]])
#######################################




#######################################
# list the "classes":
lbl = names(s_feat_list_all)
for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_train_all[[l]][1]))) # features and train errors
for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test_all[[l]][1]))) # features and test errors
for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test2_all[[l]][1]))) # features and test errors

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
print(errPred2[[1]])

#######################################



