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

sink(file="r.output.txt",split=TRUE,append = TRUE)
print(timestamp())

VALIDATION = 5.2
VERBOSE = 1
#DATA_RELOAD = 1
#wip.R = "wip.000.R"

#DATA_SET = "CV-1"
#DATA_SET = "CV-2"
#DATA_SET = "TEST-1"
#DATA_SET = "TEST-2"
DATA_SET = "CV-TOTAL"

train.bak = getDataT(DATA_SET,"train")
test.bak = getDataT(DATA_SET,"test")

pred_test_all_list = list()
pred_test_all.bak_list = list()
df.test.target_list = list()

mean_pred_test_combined_list = list()

# Split Clients in approx equal clusters of less than nCli Clients
nCli = 50000
total_pred_test = as.data.frame(0*cbind(1:nrow(test.bak),1:nrow(test.bak)))
colnames(total_pred_test) = c("id","val")
total_pred_test$id = test.bak$id
# Randomize the Clients:
all_Cli = unique(train.bak$Cliente_ID)
num_Cli = length(all_Cli)
set.seed(2300)
rnd_Cli = sample(all_Cli,num_Cli)
# All sequence: 
ssq = 1:ceiling(num_Cli/nCli)
# Just a sample: ssq = 1
# ssq = 3:10
# ssq = 2
# ssq = c(1,5,8,9,16,18) # the worst
#ssq = 3

for (jBin in ssq)
{
  print(c("jBin ",jBin," out of ",length(ssq), " just Started ",timestamp()))
  jMin = (jBin-1)*nCli+1
  jMax = min(jBin*nCli,num_Cli)
  clusterCli = rnd_Cli[jMin:jMax]
  
  idxTrain   = which(train.bak$Cliente_ID %in% clusterCli)
  idxTest    = which(test.bak$Cliente_ID %in% clusterCli)
  
  train    = train.bak[idxTrain,]
  test     = test.bak[idxTest,]
  if (DATA_SET=="CV-1" | DATA_SET=="CV-2")
    source("assmbl.wip.002.R")
  if (DATA_SET=="TEST-1" | DATA_SET=="TEST-2")
    source("assmbl.wip.002-test.R")
  if (DATA_SET=="CV-TOTAL")
    source("assmbl.wip.002-cv-total.R")
  # TODO:  
#  idx = which(total_pred_test$id %in% test$id)
#  total_pred_test[idx,]$val = pred_test
#}


  total_pred_test[idxTest,]$id = test$id
  total_pred_test[idxTest,]$val = mean_pred_test_combined

  df.test.target_list[[jBin]] = df.test.target

  mean_pred_test_combined_list[[jBin]] = mean_pred_test_combined
  
  gc()
}

err_total = errMeasure3(total_pred_test$val,test.bak$Demanda_uni_equil)
print(c("total err:",err_total))
# Save the CV-1 results:
saveDataT(total_pred_test,DATA_SET,"prediction",compress = TRUE)
sink()






