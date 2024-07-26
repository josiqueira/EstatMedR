sink("exemplo04_normal.txt")
m <- 2800
dp <- 500
cat("X ~ Normal(",m,";",dp,")\n\n", sep="")
cat("P(X < ",m-3*dp,") = ",
    round(pnorm(q=m-3*dp,mean=m,sd=dp),4),"\n", sep="")
cat("P(X < ",m-2*dp,") = ",
    round(pnorm(q=m-2*dp,mean=m,sd=dp),4),"\n", sep="")
cat("P(X < ",m-dp,") = ",
    round(pnorm(q=m-dp,mean=m,sd=dp),4),"\n", sep="")
cat("P(X < ",m,") = ",
    round(pnorm(q=m,mean=m,sd=dp),4),"\n", sep="")
cat("P(X < ",m+dp,") = ",
    round(pnorm(q=m+dp,mean=m,sd=dp),4),"\n", sep="")
cat("P(X < ",m+2*dp,") = ",
    round(pnorm(q=m+2*dp,mean=m,sd=dp),4),"\n", sep="")
cat("P(X < ",m+3*dp,") = ",
    round(pnorm(q=m+3*dp,mean=m,sd=dp),4),"\n", sep="")
cat("\nP(",m-dp," < X < ",m+dp,") = ",
    pnorm(q=m+dp,mean=m,sd=dp)-pnorm(q=m-dp,mean=m,sd=dp),"\n", sep="")
cat("P(",m-2*dp," < X < ",m+2*dp,") = ",
    pnorm(q=m+2*dp,mean=m,sd=dp)-pnorm(q=m-2*dp,mean=m,sd=dp),"\n", sep="")
qnorm(p=0.975,mean=0,sd=1)
cat("P(",m-1.96*dp," < X < ",m+1.96*dp,") = ",
    pnorm(q=m+1.96*dp,mean=m,sd=dp)-pnorm(q=m-1.96*dp,mean=m,sd=dp),"\n", sep="")
cat("P(",m-3*dp," < X < ",m+3*dp,") = ",
    pnorm(q=m+3*dp,mean=m,sd=dp)-pnorm(q=m-3*dp,mean=m,sd=dp),"\n", sep="")
qnorm(p=0.75,mean=0,sd=1)
cat("P(",m-0.675*dp," < X < ",m+0.675*dp,") = ",
    pnorm(q=m+0.675*dp,mean=m,sd=dp)-pnorm(q=m-0.675*dp,mean=m,sd=dp),"\n", sep="")
sink()
png("exemplo04_normal.png")
rn <- rnorm(n=1e6,mean=m,sd=dp)
plot(density(rn),xlab="MCTRN(g)",ylab="densidade",
     main=paste0("Distribuicao normal(",m,";",dp,")"))
dev.off()
