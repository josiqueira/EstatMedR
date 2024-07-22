n <- 100
ep <- as/sqrt(n)
qt <- qt(0.975,n-1)
ic95.lb <- am-qt*ep
ic95.ub <- am+qt*ep
cat("\nn = ",n,sep="")
cat("\nqt(0.975,n-1) = ",qt,sep="")
cat("\nEP = s/sqrt(n) = ",ep,sep="")
cat("\nIC95 = ",am," +- ",qt,"(0.975,",n-1,") * ",as,"/sqrt(",n,")",sep="")
cat("\nIC95 = [",ic95.lb,",",ic95.ub,"]",sep="")
cat("\n")
