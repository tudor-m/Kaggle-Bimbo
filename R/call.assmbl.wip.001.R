library(data.table)
source("futil.R")
# VALIDATION:
# 0 - TEST
# 1 - CV total
# 2 - CV with a small set
# 3 - CV in a loop
# 4 - mini Validation
# 5.1 - Global Validation: Train(semana 3-7), CV(8)
# 5.2 - Global Validation: Train(semana 3-8), CV(9)
# 5.3 - Submissions:       Train (3-9), Test(10)

VALIDATION = 5.2
VERBOSE = 1
#DATA_RELOAD = 1
#wip.R = "wip.000.R"

DATA_SET = "CV-1"
train.bak = getDataT(DATA_SET,"train")
test.bak = getDataT(DATA_SET,"test")

# Split Clients in approx equal clusters of less than nCli Clients
nCli = 50000
total_pred_test = as.data.frame(0*cbind(1:nrow(test.bak),1:nrow(test.bak)))
colnames(total_pred_test) = c("id","val")
# Randomize the Clients:
all_Cli = unique(train.bak$Cliente_ID)
num_Cli = length(all_Cli)
set.seed(2300)
rnd_Cli = sample(all_Cli,num_Cli)
# All sequence: 
# ssq = 1:ceiling(num_Cli/nCli)
# Just a sample: ssq = 1
# ssq = 3:10
# ssq = 2
ssq = 7
for (jBin in ssq)
{
  print(c("jBin ",jBin," out of ",ssq, " just Started"))
  jMin = (jBin-1)*nCli+1
  jMax = min(jBin*nCli,num_Cli)
  clusterCli = rnd_Cli[jMin:jMax]
  
  idxTrain   = which(train.bak$Cliente_ID %in% clusterCli)
  idxTest    = which(test.bak$Cliente_ID %in% clusterCli)
  
  train    = train.bak[idxTrain,]
  test     = test.bak[idxTest,]

  source("assmbl.wip.000.R")
# TODO:  
#  idx = which(total_pred_test$id %in% test$id)
#  total_pred_test[idx,]$val = pred_test
#}


  total_pred_test[idxTest,]$id = test$id
  total_pred_test[idxTest,]$val = pred_test
  
}

err_total = errMeasure3(total_pred_test$val,test.bak$Demanda_uni_equil)
print(c("total err:",err_total))






