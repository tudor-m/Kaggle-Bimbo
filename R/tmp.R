test2$errP = errPred2[[2]]
plot(test2$errP)
idx = which(abs(test2$errP) > 1.5)
length(idx)
