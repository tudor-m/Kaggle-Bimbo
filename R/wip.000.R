## R version of most popular local hotels
library(bit64)
library(lava)
library(data.table)
library(ggplot2)
library(caret)

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
  nCli = 10000;
  set.seed(2300)
  trainCli = sample(unique(train$Cliente_ID),nCli)
  trainWeeks = c(3,4,5,6,7,8)
  testWeeks = c(9)
  idxTrain = which(train$Semana %in% trainWeeks)
  idxTest = which(train$Semana %in% testWeeks)
  test = train[idxTest,]
  train = train[idxTrain,]
  train$id = 1:nrow(train)
  test$id = 1:nrow(test)
}

if (VALIDATION == 2) # short set of train/test for quick CV
{
  nCli = 10000;
  set.seed(2300)
  trainCli = sample(unique(train$Cliente_ID),nCli)
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
s_class_all = list()
s_feat_train_all = list()
s_feat_test_all = list()
s_err_train_all = list()
s_err_test_all = list()
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)
print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
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
s_feat_test[which(is.na(s_feat_test$V1))] = bk

s_err_train = errMeasure(s_feat_train$V1,train$Demanda_uni_equil)
s_err_test = errMeasure(s_feat_test$V1,test$Demanda_uni_equil)

print(s_err_train[[1]])
print(s_err_test[[1]])

s_class_all[[class_name]] = s_class
s_feat_train_all[[class_name]] = s_feat_train
s_feat_test_all[[class_name]] = s_feat_test
s_err_train_all[[class_name]] = s_err_train
s_err_test_all[[class_name]] = s_err_test
s_feat_list_all[[class_name]] = unlist(s_feat_list)
#######################################


#######################################
# list the "classes":
lbl = names(s_feat_list_all)
for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_train_all[[l]][1]))) # features and train errors
for (l in lbl) print(c(l,s_feat_list_all[[l]],unlist(s_err_test_all[[l]][1]))) # features and test errors

#######################################




##################
# Assembling:

df.test <- data.frame(id=test$id)
for (j in 1:length(s_feat_test_all))
  df.test[names(s_feat_test_all[j])] = s_feat_test_all[[j]]$V1

# try lm:
fit = lm(test$Demanda_uni_equil ~ .,data=df.test)

#######################################



#########################
ret = 0
coefSum = 0
coef = c(2.5,4,1.2,1.5,1.75,3.5,3.5,2)
for (i in c(1,2,3,5,6,7,8))
{
  #coef = 1/(0.2+(as.numeric(estRetE[[i]][1]))^4)
  coefSum = coefSum + coef[i]
  ret = ret + coef[i]*estRet[[i]]
}
ret = ret/coefSum

estE = errMeasure(ret,test$Demanda_uni_equil)
print(estE[[1]])
for (i in 1:length(estRetE)) print(estRetE[[i]][1])
#################






# Class A:
class_name   = "A"
s_feat_list = c("x_100m","y_100m")

#s_class <- train[,list(min(x),mean(x)-sd(x),mean(x),mean(x)+sd(x),max(x),
#                       min(y),mean(y)-sd(y),mean(y),mean(y)+sd(y),max(x),.N),by=list(x_10m,y_10m,place_id)]
#s_class <- train[,.N,by=list(x_10m,y_10m,place_id)]
s_class <- train[,list(.N),by=list(x_100m,y_100m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_100m,y_100m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_100m,y_100m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_100m,y_100m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class B:
class_name   = "B"
s_feat_list = c("x_100m","y_10m")

#s_class <- train[,list(min(x),mean(x)-sd(x),mean(x),mean(x)+sd(x),max(x),
#                       min(y),mean(y)-sd(y),mean(y),mean(y)+sd(y),max(x),.N),by=list(x_10m,y_10m,place_id)]
#s_class <- train[,.N,by=list(x_10m,y_10m,place_id)]
s_class <- train[,list(.N),by=list(x_100m,y_10m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_100m,y_10m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_100m,y_10m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_100m,y_10m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class C:
class_name   = "C"
s_feat_list = c("x_km","y_km")

#s_class <- train[,list(min(x),mean(x)-sd(x),mean(x),mean(x)+sd(x),max(x),
#                       min(y),mean(y)-sd(y),mean(y),mean(y)+sd(y),max(x),.N),by=list(x_10m,y_10m,place_id)]
#s_class <- train[,.N,by=list(x_10m,y_10m,place_id)]
s_class <- train[,list(.N),by=list(x_km,y_km,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_km,y_km)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_km,y_km)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_km,y_km)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class D:
class_name   = "D"
s_feat_list = c("x_10m","y_10m")

#s_class <- train[,list(min(x),mean(x)-sd(x),mean(x),mean(x)+sd(x),max(x),
#                       min(y),mean(y)-sd(y),mean(y),mean(y)+sd(y),max(x),.N),by=list(x_10m,y_10m,place_id)]
#s_class <- train[,.N,by=list(x_10m,y_10m,place_id)]
s_class <- train[,list(.N),by=list(x_10m,y_10m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_10m,y_10m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_10m,y_10m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_10m,y_10m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class E:
class_name   = "E"
s_feat_list = c("x_20m","y_5m")

s_class <- train[,list(.N),by=list(x_20m,y_5m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_5m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_5m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_5m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class F:
class_name   = "F"
s_feat_list = c("x_5m","y_20m")

s_class <- train[,list(.N),by=list(x_5m,y_20m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_5m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_5m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_5m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class G:
class_name   = "G"
s_feat_list = c("x_20m","y_5m","hour")

s_class <- train[,list(.N),by=list(x_20m,y_5m,hour,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_5m,hour)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_5m,hour)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_5m,hour)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class H:
class_name   = "H"
s_feat_list = c("x_5m","y_20m","hour")

s_class <- train[,list(.N),by=list(x_5m,y_20m,hour,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_5m,y_20m,hour)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_5m,y_20m,hour)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_5m,y_20m,hour)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class I:
class_name   = "I"
s_feat_list = c("x_20m","y_5m","hour","weekday")

s_class <- train[,list(.N),by=list(x_20m,y_5m,hour,weekday,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_5m,hour,weekday)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_5m,hour,weekday)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_5m,hour,weekday)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class J:
class_name   = "J"
s_feat_list = c("x_5m","y_20m","hour","weekday")

s_class <- train[,list(.N),by=list(x_5m,y_20m,hour,weekday,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_5m,y_20m,hour,weekday)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_5m,y_20m,hour,weekday)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_5m,y_20m,hour,weekday)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class K:
class_name   = "K"
s_feat_list = c("x_20m","y_5m")

s_class <- train[,mean(logac),by=list(x_20m,y_5m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,V1,10),by=list(x_20m,y_5m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,V1,10),by=list(x_20m,y_5m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,V1,10),by=list(x_20m,y_5m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class L:
class_name   = "L"
s_feat_list = c("x_5m","y_20m")

s_class <- train[,mean(logac),by=list(x_5m,y_20m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,V1,10),by=list(x_5m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,V1,10),by=list(x_5m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,V1,10),by=list(x_5m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class M:
class_name   = "M"
s_feat_list = c("x_20m","y_5m")

train_place_id_count = train[,.N,by=list(place_id)]
setnames(train_place_id_count,c("place_id","countN"))
s_class <- train[,.N,by=list(x_20m,y_5m,place_id)]
s_class <- merge(s_class,train_place_id_count,by="place_id")
s_class$prcN <- s_class$N/s_class$countN
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,prcN,10),by=list(x_20m,y_5m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,prcN,10),by=list(x_20m,y_5m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,prcN,10),by=list(x_20m,y_5m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class N:
class_name   = "N"
s_feat_list = c("x_5m","y_20m")

train_place_id_count = train[,.N,by=list(place_id)]
setnames(train_place_id_count,c("place_id","countN"))
s_class <- train[,.N,by=list(x_5m,y_20m,place_id)]
s_class <- merge(s_class,train_place_id_count,by="place_id")
s_class$prcN <- s_class$N/s_class$countN
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,prcN,10),by=list(x_5m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,prcN,10),by=list(x_5m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,prcN,10),by=list(x_5m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class Q:
class_name   = "Q"
s_feat_list = c("x_5m","y_5m")

#s_class <- train[,list(min(x),mean(x)-sd(x),mean(x),mean(x)+sd(x),max(x),
#                       min(y),mean(y)-sd(y),mean(y),mean(y)+sd(y),max(x),.N),by=list(x_10m,y_10m,place_id)]
#s_class <- train[,.N,by=list(x_10m,y_10m,place_id)]
s_class <- train[,list(.N),by=list(x_5m,y_5m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_5m,y_5m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_5m,y_5m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_5m,y_5m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class R:
class_name   = "R"
s_feat_list = c("x_20m","y_20m")

s_class <- train[,list(.N),by=list(x_20m,y_20m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class S:
class_name   = "S"
s_feat_list = c("x_20m","y_1m")

s_class <- train[,list(.N),by=list(x_20m,y_1m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_1m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_1m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_1m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class T:
class_name   = "T"
s_feat_list = c("x_1m","y_20m")

s_class <- train[,list(.N),by=list(x_1m,y_20m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_1m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_1m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_1m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class U:
class_name   = "U"
s_feat_list = c("x_20m","y_5m","hr3")

s_class <- train[,list(.N),by=list(x_20m,y_5m,hr3,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_5m,hr3)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_5m,hr3)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_5m,hr3)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class V:
class_name   = "V"
s_feat_list = c("x_5m","y_20m","hr3")

s_class <- train[,list(.N),by=list(x_5m,y_20m,hr3,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_5m,y_20m,hr3)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_5m,y_20m,hr3)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_5m,y_20m,hr3)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################


#######################################
# Class X:
class_name   = "X"
s_feat_list = c("x_100m","y_10m","hr3")

s_class <- train[,list(.N),by=list(x_100m,y_10m,hr3,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_100m,y_10m,hr3)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_100m,y_10m,hr3)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_100m,y_10m,hr3)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class Y:
class_name   = "Y"
s_feat_list = c("x_10m","y_100m","hr3")

s_class <- train[,list(.N),by=list(x_10m,y_100m,hr3,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_10m,y_100m,hr3)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_10m,y_100m,hr3)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_10m,y_100m,hr3)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class AA:
class_name   = "AA"
s_feat_list = c("x_50m","y_500m","hr12")

s_class <- train[,list(.N),by=list(x_50m,y_500m,hr12,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_50m,y_500m,hr12)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_50m,y_500m,hr12)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_50m,y_500m,hr12)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class AB:
class_name   = "AB"
s_feat_list = c("x_500m","y_50m","hr12")

s_class <- train[,list(.N),by=list(x_500m,y_50m,hr12,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_500m,y_50m,hr12)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_500m,y_50m,hr12)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_500m,y_50m,hr12)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class AC:
class_name   = "AC"
s_feat_list = c("x_100m","y_10m","month")

s_class <- train[,list(.N),by=list(x_100m,y_10m,month,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_100m,y_10m,month)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_100m,y_10m,month)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_100m,y_10m,month)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class Rb:
class_name   = "Rb"
s_feat_list = c("x_20m","y_20m")

tmp1 = train[,.N,by=list(x_20m,y_20m,place_id)]
tmp2 = train[,.N,by=list(place_id)]
tmp3 = merge(tmp1,tmp2,by="place_id")

s_class <- train[,list(.N),by=list(x_20m,y_20m,place_id)]
s_class <- merge(s_class,tmp3,by=c("place_id","x_20m","y_20m"))
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_20m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_20m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_20m,y_20m)]
timestamp()
s_cluster_bprc <- s_class[,top_bprc_kk(place_id,N,10,bproc),by=list(x_20m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_bprc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class AD:
class_name   = "AD"
s_feat_list = c("x_500m","y_20m")

s_class <- train[,list(.N),by=list(x_500m,y_20m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_500m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_500m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_500m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################

#######################################
# Class AE:
class_name   = "AE"
s_feat_list = c("x_200m","y_20m")

s_class <- train[,list(.N),by=list(x_200m,y_20m,place_id)]
timestamp()
s_cluster_seq <- s_class[,top_kk(place_id,N,10),by=list(x_200m,y_20m)]
timestamp()
s_cluster_score <- s_class[,top_score_kk(place_id,N,10),by=list(x_200m,y_20m)]
timestamp()
s_cluster_prc <- s_class[,top_prc_kk(place_id,N,10),by=list(x_200m,y_20m)]
timestamp()

s_id_cluster_seq <- merge(test,s_cluster_seq,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_seq,c("row_id","cluster_seq"))
s_id_cluster_score <- merge(test,s_cluster_score,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_score,c("row_id","cluster_score"))
s_id_cluster_prc <- merge(test,s_cluster_prc,by=s_feat_list,all.x=TRUE)[order(row_id),list(row_id,V1)]
setnames(s_id_cluster_prc,c("row_id","cluster_prc"))

s_dd_seq = as.data.frame(s_id_cluster_seq)
fName = paste(class_name,"_s_seq.csv",sep = "");write.csv(s_dd_seq, file=fName, row.names=FALSE)
s_dd_score = as.data.frame(s_id_cluster_score)
fName = paste(class_name,"_s_score.csv",sep = "");write.csv(s_dd_score, file=fName, row.names=FALSE)
s_dd_prc = as.data.frame(s_id_cluster_prc)
fName = paste(class_name,"_s_prc.csv",sep = "");write.csv(s_dd_prc, file=fName, row.names=FALSE)
#######################################


# try grid to find the best params
estRet = list();
estRet0 = list();
estGrid = list();

#for (prm in c("A","B","D","E","F","G","H","I","J","K","L","M","N","Q","R","S","T","U","V","X","Y","AA","AB","AC"))
for (prm in c("AE"))
{
  params = paste(c(prm, " 1 C 0.01 "),collapse = " "); print(params);

########################################

#params = "A 0.5 B 1.0       C 0.01" # 0.348030596682419
#params = "A 0.5 B 1.0 D 0.8 C 0.01" # 0.384122833791672
#params = "A 0.8 B 1.0 D 1.5 C 0.01" # 0.39362668519067
#params = "A 1.0 B 1.0 D 1.5 C 0.01" # 0.390993354714123
#params = "A 0.6 B 0.6 D 0.8 C 0.01" # 0.3883455
#params = "A 0.6 B 0.6 D 1.1 C 0.01" # 0.3943643
#params = "A 0.6 B 0.6 D 1.4 C 0.01" # 0.3969154
#params = "A 0.6 B 0.8 D 0.8 C 0.01" # 0.3854395
#params = "A 0.6 B 0.8 D 1.1 C 0.01" # 0.3928231
#params = "A 0.6 B 0.8 D 1.4 C 0.01" # 0.3960644
#params = "A 0.6 B 1.0 D 0.8 C 0.01" # 0.3817084
#params = "A 0.6 B 1.0 D 1.1 C 0.01" # 0.3906833
#params = "A 0.6 B 1.0 D 1.4 C 0.01" # 0.3947414
#params = "A 0.6 B 1.4 D 0.8 C 0.01" # 0.37273
#params = "A 0.6 B 1.4 D 1.1 C 0.01" # 0.3849941
#params = "A 0.6 B 1.4 D 1.4 C 0.01" # 0.3910011
#params = "A 0.6 B 2.0 D 0.8 C 0.01" # 0.3570476
#params = "A 0.6 B 2.0 D 1.1 C 0.01" # 0.3744507
#params = "A 0.6 B 2.0 D 1.4 C 0.01" # 0.3834924
#params = "A 0.8 B 0.6 D 0.8 C 0.01" # 0.3816596
#params = "A 0.8 B 0.6 D 1.1 C 0.01" # 0.3904607
#params = "A 0.8 B 0.6 D 1.4 C 0.01" # 0.3944332
#params = "A 0.8 B 0.8 D 0.8 C 0.01" # 0.3790553
#params = "A 0.8 B 0.8 D 1.1 C 0.01" # 0.3890995
#params = "A 0.8 B 0.8 D 1.4 C 0.01" # 0.3936869
#params = "A 1.0 B 0.8 D 2.0 C 0.01" # 0.395882769518992
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 0.4" # 0.417654814010682
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 0.4 F 0.2" #0.419578784402348
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 0.6 F 0.4" #0.422525233692637
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 0.8 F 0.6" #0.424432013247469
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 0.8 F 0.6 G 0.8" #0.433309501721339
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 0.8 F 0.6 G 0.8 H 0.8" #0.437059960937206
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 1.0 F 0.8 G 1.0 H 1.0" #0.437082464394168
#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 1.0 F 0.8 G 1.0 H 1.0 I 1.0 J 1.0" #.437313287714967

#params = "A 0.6 B 0.6 D 1.4 C 0.01 E 1.0 F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0.43835603977248

#params = "A 0.6 B 0.6 D 4.0 C 0.01 E 1.0 F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0.437473518342823

#params = "A 0.6 B 0.6 D 0.5 C 0.01 E 1.0 F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0.4361711
  # after bug fix:
#params = "A 0.6 B 0.6 D 0.5  C 0.01  E 1.0  F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0.4377005 -> 0.451961785866803
#params = "A 0.6 B 0.6 D 1.0  C 0.01  E 1.0  F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0  0.453546511057242
#params = "A 0.6 B 0.6 D 2.0  C 0.01  E 1.0  F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0.4387098 -> 0.454592374114073
#params = "A 0.6 B 0.6 D 2.5  C 0.01  E 1.0  F 0.8 G 1.0 H 1.0 I 1.0 J 1.0 K 0.6 L 0.6" #0.4386367 -> 0.454516577151467
#params = "A 0.6 B 0.6 D 5.0  C 0.01  E 0.4  F 0.8 G 1.5 H 1.5 I 0.5 J 0.5 K 1.0 L 1.0" #0.435326894036784

#params = "A 0.2 B 0.4 D 0.45 C 0.004 E 0.45 F 0.4 G 0.3 H 0.3 I 0.1 J 0.1 K 0.1 L 0.1"                              # 0.456648188203313
#params = "A 0.2 B 0.4 D 0.45 C 0.004 E 0.45 F 0.4 G 0.3 H 0.3 I 0.1 J 0.1 K 0.1 L 0.1 U 0.4 V 0.4 X 0.5 Y 0.4"      # 0.477563969234794
#params = "A 0.04 B 0.16 D 0.20 C 0.001 E 0.20 F 0.16 G 0.1 H 0.1                     U 0.16 V 0.16 X 0.25 Y 0.16"              # 0.479717626063021
   
  params = "A 0.04 B 0.16 D 0.40 C 0.001 E 0.40 F 0.16 G 0.1 H 0.1                     U 0.16 V 0.16 X 0.50 Y 0.16"              # 0.48273252230843
  
  #params = "A 0.04 B 0.16 D 0.50 C 0.001 E 0.50 F 0.16 G 0.1 H 0.1                     U 0.16 V 0.16 X 0.60 Y 0.16"              # 0.482621047022213
  #params = "A 0.04 B 0.16 D 0.40 C 0.001 E 0.40 F 0.16 G 0.1 H 0.1                     U 0.16 V 0.16 X 0.50 Y 0.16              AE 0.1" # 0.482202125337483
  #params = "A 0.04 B 0.16 D 0.20 C 0.001 E 0.20 F 0.16 G 0.1 H 0.1                     U 0.16 V 0.16 X 0.25 Y 0.16 AC 0.04"      # 0.479674962681876
  
#params = "A 0.2 B 0.4 D 0.45 C 0.004 E 0.45 F 0.4 G 0.3 H 0.3 I 0.1 J 0.1 K 0.1 L 0.1 U 0.4 V 0.4 X 0.5 Y 0.4 AC 0.35"      # 0.477142970209535

#params = "A 0.2 B 0.4 D 0.45 C 0.004 E 0.45 F 0.4 G 0.3 H 0.3 I 0.1 J 0.1 K 0.1 L 0.1 U 0.4 V 0.4 X 0.5 Y 0.4 AC 0.20"      # 0.477560333888866
  
#params = "A 0.2 B 0.4 D 0.90 C 0.004 E 0.90 F 0.8 G 0.3 H 0.3 I 0.1 J 0.1 K 0.1 L 0.1"                              # 0.455391397182368
#params = "A 0.2 B 0.4 D 0.45 C 0.004 E 0.45 F 0.4 G 0.3 H 0.3                        "                              # 0.45515390522622
#params = "A 0.2 B 0.4 D 0.45 C 0.004 E 0.45 F 0.4 G 0.3 H 0.3                                              R 0.43"  #0.45641897964253
#params = "                   C 0.004                                                                                 S 0.35 T 0.35"  #00.388628217274655
  
  
  #params = "C 0.01"     #0.0041925405921428

  #params = "A 1 C 0.01" #0.227409505982974
  #params = "B 1 C 0.01" #0.3840512
  #params = "D 1 C 0.01" #0.4339639
  #params = "E 1 C 0.01" #0.4337672
  #params = "F 1 C 0.01" #0.419227
  #params = "G 1 C 0.01" #0.2962402
  #params = "H 1 C 0.01" #0.2836872
  #params = "I 1 C 0.01" #0.1245319
  #params = "J 1 C 0.01" #0.1175754
  #params = "K 1 C 0.01" #0.12663
  #params = "L 1 C 0.01" #0.1194116  
  #params = "M 1 C 0.01" #0.36936470096761
  #params = "N 1 C 0.01" #0.352560262480285
  #params = "Q 1 C 0.01" #0.377673491820913
  #params = "R 1 C 0.01" #0.435650585921687
  #params = "S 1 C 0.01" #0.3575213
  #params = "T 1 C 0.01" #0.3418761
  #
  #
  
# Call the ensembler:
wrkDir = "/Users/user/Projects/Kaggle/Kaggle-Facebook/C/Build/Products/Debug/"
cmd = "ensembler "
cmdLine = paste(c(wrkDir,cmd,params),collapse = "")
cmdLineCode = paste(unlist(strsplit(params,split=" ")),collapse = "")
timestamp()
system(cmdLine)
timestamp()
########################################

# The result is in "out.csv", make  copy for future ensembling:
if (validation == 1)
{
  fn = paste(c("v_",cmdLineCode,".csv"),collapse="")
  system(paste(c("cp out.csv ",fn),collapse=""))
  # if you want to reload the data produced by fn, update fn and run:
  # dd=loadSubmission(fn)
  # system(paste(c("cp ",fn," out.csv "),collapse=""))

}
if (validation == 0)
{
  fn = paste(c("p_",cmdLineCode,".csv"))
  system(paste(c("cp out.csv ",fn),collapse=""))
}

if (validation == 0)
{
  subFile = paste(c("submission",subName),collapse="")
  system(paste(c("cp out.csv ",subFile),collapse=""))
}

# Error estimation:
dd = loadSubmission("out.csv")
dd$V5 <- NULL  # quick bug workaround

if (validation == 1)
{
  estE = errMeasure1(dd,test$place_id)
  estE0 = errMeasure1(dd,test$place_id,idx0)
  #print(c("error:", estE[[1]],count(estE[[2]])))
  print(c("error:", estE))
  print(c("error:", estE0))
}

estRet[[params]] = estE;
estRet0[[params]] = estE0;
# Grid error (1km square cells):
estRetGrid = matrix(nrow=10, ncol=10)
for (ii in (1:10)) for (jj in (1:10))
{
  x1=ii-1; x2=ii;
  y1=jj-1; y2=jj;
  idx_xy = which (test$x>=x1 & test$x<=x2 & test$y>=y1 & test$y<=y2)
  estRetGrid[ii,jj] = errMeasure1(dd,test$place_id,idx_xy)
}
estGrid[[params]] = estRetGrid;
}
save(estGrid,file="estGrid.rda")

# IMPORTANT:  Missed points analysis:
# Totally missed - small analysis:
idx0 = which((dd$V2 != test$place_id) & (dd$V3!=test$place_id) & (dd$V4!=test$place_id))
idx123 = which((dd$V2 == test$place_id) | (dd$V3 == test$place_id) | (dd$V4 == test$place_id))
estE0 = errMeasure1(dd,test$place_id,idx0) # missed points error
estE123 = errMeasure1(dd,test$place_id,idx123) # guessed points error

# The most missed place_id:
Nmax = max(test[idx0,.N,by=place_id]$N)
Imax_sort = sort(test[idx0,.N,by=place_id]$N, decreasing = TRUE)
Imax = which(test[idx0,.N,by=place_id]$N == Imax_sort[1]) # change to 2, 3, etc
pid = test[idx0,.N,by=place_id][Imax]$place_id
idxpid = which(test$place_id == pid) # all the lines with that place
idx0pid = idxpid[which(idxpid %in% idx0)] # all the missed lines with that place
idxtrainpid = which(train$place_id == pid)
plot(train[idxtrainpid,]$x ,train[idxtrainpid,]$y)
plot(test[idx0pid,]$x ,test[idx0pid,]$y)
plot(train[idxtrainpid,]$time ,train[idxtrainpid,]$x)
plot(train[idxtrainpid,]$time ,train[idxtrainpid,]$y)
plot(test[idx0pid,]$time ,test[idx0pid,]$x)
plot(test[idx0pid,]$time ,test[idx0pid,]$y)

# Grid error (1km square cells):
estRetGrid = matrix(nrow=10, ncol=10)
for (ii in (1:10)) for (jj in (1:10))
{
  x1=ii-1; x2=ii;
  y1=jj-1; y2=jj;
  idx_xy = which (test$x>=x1 & test$x<=x2 & test$y>=y1 & test$y<=y2)
  estRetGrid[ii,jj] = errMeasure1(dd,test$place_id,idx_xy)
}



max((estRetGrid))

# combine:
orderSubmission("submission.public.005.1.csv")
combineSubmission("submission.wip.005.A.csv",c("submission.wip.005.csv 0.1 submission.public.005.1.csv 0.9"))
ss1 = loadSubmission("submission.wip.005.A.csv");    # submission combined
ss1$V5 <- NULL
ss2 = loadSubmission("submission.public.005.1.csv") # public
ss3 = loadSubmission("submission.wip.005.csv")      # produced
ss3$V5 <- NULL
# 0.58374

# combine:
orderSubmission("submission.public.005.1.csv")
combineSubmission("submission.wip.005.B.csv",c("submission.wip.005.csv 0.2 submission.public.005.1.csv 0.8"))
ss1 = loadSubmission("submission.wip.005.B.csv");    # submission combined
ss1$V5 <- NULL
ss2 = loadSubmission("submission.public.005.1.csv") # public
ss3 = loadSubmission("submission.wip.005.csv")      # produced
ss3$V5 <- NULL
# 0.58350

# combine only the border 100m:
orderSubmission("submission.public.005.1.csv")
combineSubmission("submission.wip.005.A.csv",c("submission.wip.005.csv 0.1 submission.public.005.1.csv 0.9"))
ss1 = loadSubmission("submission.wip.005.A.csv");    # submission combined
ss1$V5 <- NULL
ss2 = loadSubmission("submission.public.005.1.csv") # public
ss3 = loadSubmission("submission.wip.005.csv")      # produced
ss3$V5 <- NULL

idx = which(test$x_100m ==0 | test$x_100m==99 | test$y_100m==0 | test$y_100m==99)
ss2[idx,] = ss1[idx,]
writeSubmission(ss2,"submission.wip.005.C.csv")
# 0.58372

# combine:
orderSubmission("submission.public.005.1.csv")
combineSubmission("submission.wip.005.D.csv",c("submission.wip.005.csv 0.15 submission.public.005.1.csv 0.85"))
ss1 = loadSubmission("submission.wip.005.A.csv");    # submission combined
ss1$V5 <- NULL
ss2 = loadSubmission("submission.public.005.1.csv") # public
ss3 = loadSubmission("submission.wip.005.csv")      # produced
ss3$V5 <- NULL
# 0.58374



