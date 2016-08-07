
## R version of most popular local hotels
library(bit64)
library(lava)
library(data.table)
library(ggplot2)
library(caret)
library(plyr)
library(MASS)
library(glmnet)
library(sgd)
library(penalized)
library(xgboost)

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
subName = ".wip.001.csv"


#######################################
#   Target-based learners:
#######################################

s_feat_list_all = list()
s_feat_train_all = list()
s_feat_test_all = list()

#######################################
saveDataT(train,DATABASE,"train")
saveDataT(test,DATABASE,"test")
#######################################
class_name   = "AA"
s_feat_list = list("Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AB"
s_feat_list = list("Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################


#######################################
class_name   = "A"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "B"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "C"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "D"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "E"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "F"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "G"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "H"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "I"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "J"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "K"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "A1"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AA1"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AB1"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################


#######################################
class_name   = "B1"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "C1"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "D1"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "E1"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "F1"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "G1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "H1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "I1"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "J1"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "K1"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "A2"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "B2"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "C2"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "D2"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "E2"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "F2"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "G2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "H2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "I2"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "J2"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "K2"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_mean

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AMAX"
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AAMAX"
s_feat_list = list("Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "ABMAX"
s_feat_list = list("Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################


#######################################
class_name   = "BMAX"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "CMAX"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "DMAX"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "EMAX"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "FMAX"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "GMAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "HMAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "IMAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################


#######################################
class_name   = "KMAX"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Demanda_uni_equil),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "A1MAX"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AA1MAX"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "AB1MAX"
#s_feat_list = list("Cliente_ID","Producto_ID","Venta_uni_hoy")
s_feat_list = list("Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################


#######################################
class_name   = "B1MAX"
s_feat_list = list("Ruta_SAK","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "C1MAX"
s_feat_list = list("Agencia_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "D1MAX"
s_feat_list = list("Ruta_SAK","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "E1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "F1MAX"
s_feat_list = list("Agencia_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "G1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "H1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "I1MAX"
s_feat_list = list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "J1MAX"
s_feat_list = list("Producto_ID","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
class_name   = "K1MAX"
s_feat_list = list("Cliente_ID","Canal_ID")
s_fct = s_fct_max

c_feat = c(unlist(s_feat_list))
s_class <- train[,list(s_fct(Venta_uni_hoy),.N),by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_train_this = s_feat_train$V1

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(id,V1)]
s_feat_test_this = s_feat_test$V1

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################

#######################################
# list the "classes":
lbl = names(s_feat_list_all)
if (VERBOSE == 1)
{
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_train_all[[l]]))) # features and train errors
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_cv_all[[l]]))) # features and test errors
  for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test_all[[l]]))) # features and test errors
}

#######################################



# Here Call assmbl.wip.xxx.R







