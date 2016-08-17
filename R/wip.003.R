
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

feat_list = list(
  list("Cliente_ID"),
  list("Producto_ID"),
  list("Cliente_ID","Producto_ID"),
  list("Ruta_SAK","Producto_ID"),
  list("Agencia_ID","Producto_ID"),
  list("Ruta_SAK","Cliente_ID"),
  list("Agencia_ID","Cliente_ID"),
  list("Agencia_ID","Ruta_SAK"),
  list("Agencia_ID","Cliente_ID","Producto_ID"),
  list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK"),
  list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID"),
  list("Producto_ID","Canal_ID"),
  list("Cliente_ID","Canal_ID")
)
  
class_name_list = c(
  "AA",
  "AB",
  "A",
  "B",
  "C",
  "D",
  "E",
  "F",
  "G",
  "H",
  "I",
  "J",
  "K"
)
s_fct = s_fct_mean
DATABASE = "TMP"
for (ii in 1:length(feat_list))
{
  #######################################
  class_name   = class_name_list[[ii]]
  s_feat_list = feat_list[[ii]]
  s_fct = s_fct
  
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
}
for (ii in 1:length(feat_list))
{
  for (S in SemanaList) {
    train.swp = train
    train = train.swp[Semana==S,]
    
    #######################################
    class_name   = class_name_list[[ii]]
    class_name   = paste(class_name,"w",S,collapse = "",sep="")
    s_feat_list = feat_list[[ii]]
    s_fct = s_fct
    
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
    
  }}



class_name_list = c(
  "AAMAX",
  "ABMAX",
  "AMAX",
  "BMAX",
  "CMAX",
  "DMAX",
  "EMAX",
  "FMAX",
  "GMAX",
  "HMAX",
  "IMX",
  "JMAX",
  "KMAX"
)
s_fct = s_fct_max
DATABASE = "TMP"
for (ii in 1:length(feat_list))
{
  #######################################
  class_name   = class_name_list[[ii]]
  s_feat_list = feat_list[[ii]]
  s_fct = s_fct
  
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
