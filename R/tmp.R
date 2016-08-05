test2$errP = errPred2[[2]]
plot(test2$errP)
idx = which(abs(test2$errP) > 1.5)
length(idx)

for (nm in names(df.train)) print(c(nm,errMeasure(df.train[[nm]],train$Venta_uni_hoy)[[1]]))
for (nm in names(df.cv)) print(c(nm,errMeasure(df.cv[[nm]],cv$Venta_uni_hoy)[[1]]))
for (nm in names(df.test)) print(c(nm,errMeasure(df.test[[nm]],test$Venta_uni_hoy)[[1]]))

errMeasure(cv$Venta_uni_hoy,0.25*df.cv$B+0.05*df.cv$C+0.05*df.cv$D1+0.45*df.cv$G1)[[1]]
errMeasure(test$Venta_uni_hoy,0.25*df.test$B+0.05*df.test$C+0.05*df.test$D1+0.45*df.test$G1)[[1]]


source("build.data.cv-1.R")
source("build.data.cv-2.R")

tmp.t = getDataT("CV-2","train")
tmp.t$Demanda_uni_equil = expm1(tmp.t$Demanda_uni_equil)
saveDataT(tmp.t,"CV-2","train")
remove(tmp.t)
