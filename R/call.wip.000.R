library(data.table)
# VALIDATION:
# 0 - TEST
# 1 - CV total
# 2 - CV with a small set
# 3 - CV in a loop
# 4 - mini Validation

VALIDATION = 4
VERBOSE = 0
DATA_RELOAD = 1
wip.R = "wip.000.R"

jBinCv = 3;
if (VALIDATION == 3 | VALIDATION == 2 | VALIDATION == 4) # FULL CV
{
  if (DATA_RELOAD == 1) {
  train.bak <- 
    fread('../data/train.csv', header=TRUE,
          select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
  trainWeeks = c(3,4,5,6,7)
  cvWeeks = c(8)
  testWeeks = c(8)
  
  trainData <- train.bak[which(train.bak$Semana %in% trainWeeks)]
  cvData <- train.bak[which(train.bak$Semana %in% cvWeeks)]
  testData <- train.bak[which(train.bak$Semana %in% testWeeks)]
  remove(train.bak)
  trainData$id <- 1:nrow(trainData)
  cvData$id <- 1:nrow(cvData)
  testData$id <- 1:nrow(testData)
  DATA_RELOAD = 0
  }
  
  nCli = 50000;
  if (VALIDATION == 4) nCli = 5000;
  nu_Cl = length(unique(trainData$Cliente_ID))
  n_Cl = length(trainData$Cliente_ID)

  total_pred_test = data.frame(id=testData$id) # this is where the prediction will be collected
  total_pred_test$val = -1
  if (VALIDATION == 3) ssq = 1:ceiling(nu_Cl/nCli)
  if (VALIDATION == 2) ssq = jBinCv
  if (VALIDATION == 4) ssq = jBinCv
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
