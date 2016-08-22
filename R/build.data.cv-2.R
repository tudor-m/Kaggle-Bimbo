library(data.table)

# CV2 dataset

train.bak <- 
  fread('../data/train.csv', header=TRUE,
        select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
total_pred_test_CV1 = getDataT("CV-1","prediction")$val
train.bak[Semana==8,]$Demanda_uni_equil = as.integer(round(total_pred_test_CV1))

trainWeeks = c(4,5,6,7,8)
testWeeks = c(9)

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
SemanaList = trainWeeks

EXTERNAL_DATA = TRUE
DATABASE = "CV-2"
  
feat_list = list(
    list("Cliente_ID"),
    list("Producto_ID"),
#    list("Cliente_ID","Producto_ID"),
    list("Ruta_SAK","Producto_ID"),
    list("Agencia_ID","Producto_ID"),
    list("Ruta_SAK","Cliente_ID"),
#    list("Agencia_ID","Cliente_ID"),
#    list("Agencia_ID","Ruta_SAK"),
    list("Agencia_ID","Cliente_ID","Producto_ID"),
    list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK"),
#    list("Agencia_ID","Cliente_ID","Producto_ID","Ruta_SAK","Canal_ID"),
    list("Producto_ID","Canal_ID")
#    list("Cliente_ID","Canal_ID")
  )
  
  class_name_list = c(
    "AA",
    "AB",
#    "A",
    "B",
    "C",
    "D",
#    "E",
#    "F",
    "G",
    "H",
#    "I",
    "J"
#    "K"
  )
  

source("wip.003.R")
remove(train)
remove(test)
gc()
