sink("exemplo06_normal.txt")
m <- 0.07
dp <- 0.03
cat("X ~ Normal(",m,";",dp,")\n")
cat("P(X <",m-3*dp,") =",pnorm(q=m-3*dp,mean=m,sd=dp),"\n")
cat("P(X <",m-2*dp,") =",pnorm(q=m-2*dp,mean=m,sd=dp),"\n")
cat("P(X <",m-dp,") =",pnorm(q=m-dp,mean=m,sd=dp),"\n")
cat("P(X <",m,") =",pnorm(q=m,mean=m,sd=dp),"\n")
cat("P(X <",m+dp,") =",pnorm(q=m+dp,mean=m,sd=dp),"\n")
cat("P(X <",m+2*dp,") =",pnorm(q=m+2*dp,mean=m,sd=dp),"\n")
cat("P(X <",m+3*dp,") =",pnorm(q=m+3*dp,mean=m,sd=dp),"\n")
cat("P(",m-dp,"< X<",m+dp,") =",
    pnorm(q=m+dp,mean=m,sd=dp)-pnorm(q=m-dp,mean=m,sd=dp),"\n")
cat("P(",m-2*dp,"< X <",m+2*dp,") =",
    pnorm(q=m+2*dp,mean=m,sd=dp)-pnorm(q=m-2*dp,mean=m,sd=dp),"\n")
qnorm(p=0.975,mean=0,sd=1)
cat("P(",m-1.96*dp,"< X <",m+1.96*dp,") =",
    pnorm(q=m+1.96*dp,mean=m,sd=dp)-pnorm(q=m-1.96*dp,mean=m,sd=dp),"\n")
cat("P(",m-3*dp,"< X <",m+3*dp,") =",
    pnorm(q=m+3*dp,mean=m,sd=dp)-pnorm(q=m-3*dp,mean=m,sd=dp),"\n")
qnorm(p=0.75,mean=0,sd=1)
cat("P(",m-0.675*dp,"< X <",m+0.675*dp,") =",
    pnorm(q=m+0.675*dp,mean=m,sd=dp)-pnorm(q=m-0.675*dp,mean=m,sd=dp),"\n")
sink()
pdf("exemplo06_normal.pdf")
rn <- rnorm(n=1e6,mean=m,sd=dp)
plot(density(rn),xlab="Quantidade de C14 liberada sob a forma de CO2",ylab="densidade",
     main=paste("Distribuicao normal(",m,";",dp,")"))
dev.off()
