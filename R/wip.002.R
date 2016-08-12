
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
class_name   = "JMAX"
s_feat_list = list("Producto_ID","Canal_ID")
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



feat_list = c("Cliente_ID","Producto_ID","Ruta_SAK","Agencia_ID")
class_name_list = c("NA","NB","NC","ND")
for (i in 1:length(feat_list)) {
#######################################
class_name   = class_name_list[i]
s_feat_list = list(feat_list[i])

c_feat = c(unlist(s_feat_list))
s_class <- train[,.N,by=c_feat]

s_feat_list_all[[class_name]] = unlist(s_feat_list)

s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(N)]
s_feat_train_this = s_feat_train$N

s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(N)]
s_feat_test_this = s_feat_test$N

saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
remove(s_feat_train_this)
remove(s_feat_test_this)
gc()
print(paste(class_name,"done"))
#######################################
}




for (S in SemanaList) {
train.swp = train
train = train.swp[Semana==S,]

#######################################
class_name   = "AB"
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = "AA"
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = "AC"
class_name   = paste(class_name,"w",S,collapse = "",sep="")
s_feat_list = list("Agencia_ID")
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
class_name   = "AD"
class_name   = paste(class_name,"w",S,collapse = "",sep="")
s_feat_list = list("Ruta_SAK")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = "AMAX"
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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
class_name   = "JMAX"
class_name   = paste(class_name,"w",S,collapse = "",sep="")
s_feat_list = list("Producto_ID","Canal_ID")
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
class_name   = paste(class_name,"w",S,collapse = "",sep="")
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

train = train.swp
remove(train.swp)
gc()

}










for (S in SemanaList) {
  train.swp = train
  train = train.swp[Semana==S,]

  feat_list = c("Cliente_ID","Producto_ID","Ruta_SAK","Agencia_ID")
  class_name_list = c("NA","NB","NC","ND")
  for (i in 1:length(feat_list)) {
    #######################################
    class_name   = class_name_list[i]
    class_name   = paste(class_name,"w",S,collapse = "",sep="")
    s_feat_list = list(feat_list[i])
    
    c_feat = c(unlist(s_feat_list))
    s_class <- train[,.N,by=c_feat]
    
    s_feat_list_all[[class_name]] = unlist(s_feat_list)
    
    s_feat_train <- merge(train,s_class,by=c_feat,all.x=TRUE)[order(id),list(N)]
    s_feat_train_this = s_feat_train$N
    
    s_feat_test <- merge(test,s_class,by=c_feat,all.x=TRUE)[order(id),list(N)]
    s_feat_test_this = s_feat_test$N
    
    saveDataT(s_feat_train_this,DATABASE,paste("s_feat_train_all","_",class_name,sep=""))
    saveDataT(s_feat_test_this,DATABASE,paste("s_feat_test_all","_",class_name,sep=""))
    remove(s_feat_train_this)
    remove(s_feat_test_this)
    gc()
    print(paste(class_name,"done"))
    #######################################
  }
  
  train = train.swp
  remove(train.swp)
  gc()
  
}