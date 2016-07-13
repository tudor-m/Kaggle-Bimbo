library(data.table)
# VALIDATION:
# 0 - TEST
# 1 - CV total
# 2 - CV with a small set
# 3 - CV in a loop

VALIDATION = 3
VERBOSE = 0
wip.R = "wip.000.R"

if (VALIDATION == 3)
{}

if (VALIDATION == 1) # Full CV (cross-validation)
{
  train <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
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
  source(wip.R)
}

if (VALIDATION == 2) # short set of train/test for quick CV
{
  train <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  set.seed(2300)
  
  nCli = 5000;
  #nCli = 50000;
  jBin = 1;
  
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
  source(wip.R)
}

if (VALIDATION == 3) # short set of train/test for quick CV
{
  
  train.bak <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  
  nCli = 5000;
  #nCli = 50000;
  for (jBin in 1:ceiling(length(unique(train.bak$Cliente_ID))/nCli))
  {
  jMin = (jBin-1)*nCli+1
  jMax = min(jBin*nCli,nrow(train.bak))
  train <- train.bak
  set.seed(2300)

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
  print(c("jBin: ",jBin))
  source(wip.R)
  }
  remove(train.bak)
}



if (VALIDATION == 0)
{
  train <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  test <- 
    fread('../data/test.csv', header=TRUE,
          select = c("row_id","x","y","accuracy","time"))
}
