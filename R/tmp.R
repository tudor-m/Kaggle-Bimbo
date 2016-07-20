test2$errP = errPred2[[2]]
plot(test2$errP)
idx = which(abs(test2$errP) > 1.5)
length(idx)

for (nm in names(df.train)) print(c(nm,errMeasure(df.train[[nm]],train$Venta_uni_hoy)[[1]]))
for (nm in names(df.cv)) print(c(nm,errMeasure(df.cv[[nm]],cv$Venta_uni_hoy)[[1]]))
for (nm in names(df.test)) print(c(nm,errMeasure(df.test[[nm]],test$Venta_uni_hoy)[[1]]))