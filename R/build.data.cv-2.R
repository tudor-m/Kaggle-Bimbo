library(data.table)

# CV2 dataset
DATABASE = "CV-2"

train.bak <- 
  fread('../data/train.csv', header=TRUE,
        select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
trainWeeks = c(3,4,5,6,7)
testWeeks = c(8)

nrow.train.bak = nrow(train.bak)
clients.train.bak = unique(train.bak$Cliente_ID)
products.train.bak = unique(train.bak$Producto_ID )
agencies.train.bak = unique(train.bak$Agencia_ID)
canals.train.bak = unique(train.bak$Canal_ID)
routes.train.bak = unique(train.bak$Ruta_SAK)

train.bak$Demanda_uni_equil = log1p(train.bak$Demanda_uni_equil)

trainData <- train.bak[which(train.bak$Semana %in% trainWeeks)]
testData <- train.bak[which(train.bak$Semana %in% testWeeks)]
remove(train.bak)
trainData$id <- 1:nrow(trainData)
testData$id <- 1:nrow(testData)
gc()  # garbage collector

train    = trainData
test     = testData

remove(trainData)
remove(testData)
gc()



VERBOSE = 1
source("wip.001.R")


##############################################


VERBOSE = 0
DATA_RELOAD = 1
wip.R = "wip.000.R"

jBinCv = 1;
if (VALIDATION == 3 | VALIDATION == 2 | VALIDATION == 4 | VALIDATION == 5.1 | VALIDATION = 5.2) # FULL CV
{
  if (DATA_RELOAD == 1) {
  train.bak <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  trainWeeks = c(3,4,5,6,7,8)
  cvWeeks = c(9)
  testWeeks = c(9)
  
  
  if (VALIDATION == 5.1)
  {
    trainWeeks = c(3,4,5,6,7,8)
    cvWeeks = c(9)
    testWeeks = c(9)
  }
  if (VALIDATION == 5.2)
  {
    trainWeeks = c(3,4,5,6,7,8)
    cvWeeks = c(9)
    testWeeks = c(9)
  }
  
  nrow.train.bak = nrow(train.bak)
  clients.train.bak = unique(train.bak$Cliente_ID)
  products.train.bak = unique(train.bak$Producto_ID )
  agencies.train.bak = unique(train.bak$Agencia_ID)
  canals.train.bak = unique(train.bak$Canal_ID)
  routes.train.bak = unique(train.bak$Ruta_SAK)

  trainData <- train.bak[which(train.bak$Semana %in% trainWeeks)]
  cvData <- train.bak[which(train.bak$Semana %in% cvWeeks)]
  testData <- train.bak[which(train.bak$Semana %in% testWeeks)]
  remove(train.bak)
  trainData$id <- 1:nrow(trainData)
  cvData$id <- 1:nrow(cvData)
  testData$id <- 1:nrow(testData)
  DATA_RELOAD = 0
  }
  
  nu_Cl = length(unique(trainData$Cliente_ID))
  n_Cl = length(trainData$Cliente_ID)

  if (VALIDATION == 4) nCli = 5000;
  if (VALIDATION == 5.1 | VALIDATION == 5.2) nCli = n_Cl;

  total_pred_test = data.frame(id=testData$id) # this is where the prediction will be collected
  total_pred_test$val = -1
  if (VALIDATION == 3) ssq = 1:ceiling(nu_Cl/nCli)
  if (VALIDATION == 2) ssq = jBinCv
  if (VALIDATION == 4) ssq = jBinCv
  if (VALIDATION == 5.1 | VALIDATION == 5.2) ssq = 1
  for (jBin in ssq)
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
    if (VALIDATION == 5.1 | VALIDATION == 5.2) VERBOSE = 1
    if (VALIDATION == 4) VERBOSE = 1
    if (VALIDATION == 2) VERBOSE = 1
    if (VALIDATION == 3) VERBOSE = 0
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
