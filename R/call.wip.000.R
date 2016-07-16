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
  train.bak <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  
  nCli = 50000;
  nu_Cl = length(unique(train.bak$Cliente_ID))
  n_Cl = length(train.bak$Cliente_ID)
  
  
  train <- train.bak
  set.seed(2300)
  
  nCli = 50000;
  #nCli = 50000;
  jBin = 17;
  jMin = (jBin-1)*nCli+1
  jMax = min(jBin*nCli,nu_Cl)
  train <- train.bak
  set.seed(2300)
  
  trainCli = sample(unique(train$Cliente_ID),length(unique(train$Cliente_ID)))[((jBin-1)*nCli+1):(jBin*nCli)]
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
  VERBOSE = 1
  source(wip.R)
}

if (VALIDATION == 3) # FULL CV
{
  train.bak <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  trainWeeks = c(3,4,5,6,7)
  cvWeeks = c(8)
  testWeeks = c(9)
  
  trainData <- train.bak[which(train.bak$Semana %in% trainWeeks)]
  cvData <- train.bak[which(train.bak$Semana %in% cvWeeks)]
  testData <- train.bak[which(train.bak$Semana %in% testWeeks)]
  remove(train.bak)
  trainData$id <- 1:nrow(trainData)
  cvData$id <- 1:nrow(cvData)
  testData$id <- 1:nrow(testData)
  
  nCli = 50000;
  nu_Cl = length(unique(trainData$Cliente_ID))
  n_Cl = length(trainData$Cliente_ID)

  total_pred_test = data.frame(id=testData$id) # this is where the prediction will be collected
  total_pred_test$val = -1
  
  for (jBin in 1:ceiling(nu_Cl/nCli))
  {
    jMin = (jBin-1)*nCli+1
    jMax = min(jBin*nCli,nu_Cl)
    set.seed(2300)
    
    clusterCli = sample(unique(trainData$Cliente_ID),length(unique(trainData$Cliente_ID)))[((jBin-1)*nCli+1):(jBin*nCli)]

    idxTrain   = which(trainData$Cliente_ID %in% clusterCli)
    idxCv      = which(cvData$Cliente_ID %in% clusterCli)
    idxTest    = which(testData$Cliente_ID %in% clusterCli)
    
    train    = trainData[idxTrain,]
    cv       = cvData[idxCv,]
    test     = testData[idxTest,]

    print(c("jBin: ",jBin))

    source(wip.R)
    
    idx = which(total_pred_test$id %in% test$id)
    total_pred_test[idx,]$val = pred_test
  }
  idx_1 = which(total_pred_test$val==-1)
  total_pred_test[idx_1,]$val = 4 # empirical value
  err_total = errMeasure(total_pred_test$val,testData$Demanda_uni_equil)[[1]]
  print(c("total err:",err_total[[1]]))
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
