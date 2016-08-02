library(data.table)

# Submission dataset
train.bak <- 
  fread('../data/train.csv', header=TRUE,
        select = c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil"))
test.bak <- 
  fread('../data/test.csv', header=TRUE,
        select = c("id","Semana","Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID"))

trainWeeks = c(3,4,5,6,7,8,9)
testWeeks = NULL
DATABASE = "TEST"
VERBOSE = 1

nrow.train.bak = nrow(train.bak)
clients.train.bak = unique(train.bak$Cliente_ID)
products.train.bak = unique(train.bak$Producto_ID )
agencies.train.bak = unique(train.bak$Agencia_ID)
canals.train.bak = unique(train.bak$Canal_ID)
routes.train.bak = unique(train.bak$Ruta_SAK)

trainData <- train.bak
testData <- test.bak
remove(train.bak)
remove(test.bak)

trainData$id <- 1:nrow(trainData)
#testData$id <- 1:nrow(testData)
gc()  # garbage collector

train    = trainData
test     = testData

remove(trainData)
remove(testData)
gc()




source("wip.001.R")


