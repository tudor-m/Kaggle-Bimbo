library(data.table)
inDataTables = list()
inDataFiles = list()

inDataFiles[[1]] = "submit_mean_by_Agency_Ruta_Client.csv"
inDataFiles[[2]] = "public.wip.script-2.002.csv"
if (1==0)
  inCoef = c(0.8,0.2)
if (1==0)
  inCoef = c(0.85,0.15)
if (1==0)
  inCoef = c(0.75,0.25)

inCoef = c(0.7,0.3)

for (i in 1:length(inDataFiles))
{
  inDataTables[[i]] <- fread(inDataFiles[[i]], header=TRUE)
}

allData = inDataTables[[1]]
for (i in 2:length(inDataTables))
{
  allData = merge(allData,inDataTables[[2]],by="id")
}

combinedData = 0
for (i in 1:length(inCoef))
{
  combinedData = combinedData + inCoef[i]*allData[[1+i]]
}

submitData = as.data.table(cbind(allData$id,combinedData))
setnames(submitData,c("id","Demanda_uni_equil"))
options(scipen = 999)
if (1==0)
  write.csv(submitData[,.(id,Demanda_uni_equil)],"submit.LoadAndCombine.8_2.csv", row.names = FALSE)
if (1==0)
  write.csv(submitData[,.(id,Demanda_uni_equil)],"submit.LoadAndCombine.85_15.csv", row.names = FALSE)

if (1==0)
  write.csv(submitData[,.(id,Demanda_uni_equil)],"submit.LoadAndCombine.75_25.csv", row.names = FALSE)

write.csv(submitData[,.(id,Demanda_uni_equil)],"submit.LoadAndCombine.7_3.csv", row.names = FALSE)

options(scipen = 0)