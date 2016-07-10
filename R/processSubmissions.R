loadSubmission <- function (fName)
{
  fNameTmp = paste(c(fName,".tmp"),collapse="")
  system(paste(c("/Users/user/Projects/Kaggle/Kaggle-Facebook/C/Build/Products/Debug/expedia-r",fName,fNameTmp),collapse = " "))
  ret <- fread(fNameTmp, header=FALSE)
  return(ret)
}


compareSubmission <- function(fName1,fName2)
{
  s1 = loadSubmission(fName1)
  s2 = loadSubmission(fName2)
  ret = NA
  if (nrow(s1)==nrow(s2) & ncol(s1)==ncol(s2))
    ret = rowSums((s1==s2)[,2:6])
  return(list(ret,s1,s2))
}

combineSubmission <- function(outFile,lineParams)
{
  # input: (file weight)
  # tmp output in outCombine.csv
  
  cmd = "/Users/user/Projects/Kaggle/Kaggle-Facebook/C/Build/Products/Debug/combine "
  cmdLine = paste(c(cmd,lineParams),collapse = "")
  system(cmdLine)
  system(paste(c("cp","outCombine.csv",outFile),collapse = " "))
}

orderSubmission <- function(fileName)
{
  s = loadSubmission(fileName)
  s = s[order(V1)]
  s = s[,list(V1,paste(V2,V3,V4))]
  setnames(s,c("row_id","place_id"))
  write.csv(s,file=fileName,row.names=FALSE,quote=FALSE)
}

writeSubmission <- function(s,fName)
{
  s = s[,list(V1,paste(V2,V3,V4))]
  setnames(s,c("row_id","place_id"))
  write.csv(s,file=fName,row.names=FALSE,quote=FALSE)
}  
  
  
s1 = loadSubmission("outCombine.csv")
s2 = loadSubmission("submission.public.C.wip.024.csv")
s3 = loadSubmission("submission.public.C.wip.030.csv")
s4 = loadSubmission("submission.public.C.csv")

#order submission:

s12 = compareSubmission("submission.public.C.csv","submission.public.C.wip.024.csv")
#s12 = compareSubmission("submission.public.C.csv","submission.public.C.wip.030.csv")
#s12 = compareSubmission("submission.public.C.csv","submission.public.C.wip.031.csv")
#s12 = compareSubmission("submission.public.C.csv","outCombine.csv")
#s12 = compareSubmission("submission.public.C.wip.030.csv","submission.public.C.wip.031.csv")

for (i in 5:0){
idx = which(s12[[1]] == i);
print(length(idx))}

idx = which(s12[[1]] == 0);
s12[[2]][idx,]
s12[[3]][idx,]

#combineSubmission("ens_024_public_12_1_9.csv",c("submission_public_12.csv 0.9 submission.public.C.wip.024.csv 0.1")) # 0.50177
#combineSubmission("ens_024_public_12_3_7.csv",c("submission_public_12.csv 0.7 submission.public.C.wip.024.csv 0.3")) # 0.50182
#combineSubmission("ens_024_public_12_4_6.csv",c("submission_public_12.csv 0.6 submission.public.C.wip.024.csv 0.4")) # 0.50177
#combineSubmission("ens_024_public_12_25_75.csv",c("submission_public_12.csv 0.75 submission.public.C.wip.024.csv 0.25")) # 0.50182
#combineSubmission("ens_024_public_13_25_75.csv",c("submission_public_13.csv 0.75 submission.public.C.wip.024.csv 0.25")) # 0.50186
#combineSubmission("ens_024_public_14_25_75.csv",c("submission_public_14.csv 0.75 submission.public.C.wip.024.csv 0.25")) # 0.50189


                  
                  