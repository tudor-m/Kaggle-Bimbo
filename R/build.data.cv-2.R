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


