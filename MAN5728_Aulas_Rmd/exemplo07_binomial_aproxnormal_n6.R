sink("exemplo07_binomial_aproxnormal_n6.txt")
n <- 6
p <- 0.7
cat("X ~ binomial(",n,", ",p,")\n", sep="")
m <- n*p
dp <- sqrt(n*p*(1-p))
cat("Media de X = ",m,"\n", sep="")
cat("Desvio-padrao de X = ",round(dp,2),"\n", sep="")
X <- 0:n
if (n*p > 5 && n*(1-p) > 5 && abs((1/sqrt(n))*(sqrt((1-p)/p)-sqrt(p/(1-p)))) < 0.3)
{cat("\nBinomial(",n,", ",p,") semelhante a Normal(",m,", ",round(dp,2),")\n\n", sep="")} else
{cat("\nBinomial(",n,", ",p,") diferente da Normal(",m,", ",round(dp,2),")\n\n", sep="")}
probbinom <- dbinom(x=X,size=n,prob=p)
probnorm <- dnorm(x=X,mean=m,sd=dp)
print(round((tbl <- data.frame(X,probbinom,probnorm)),6))
sink()
png("exemplo07_binomial_aproxnormal_n6.png")
plot(X,probbinom,type="h",
     main=paste0("Distribuicao binomial(",n,", ",p,")\n\n"),
     xlab="X", ylab="densidade & probabilidade",
     xlim=c(-1,n+1), ylim=c(0,1.1*max(c(max(probbinom),max(probnorm)))),
     lwd=1,col="black")
points(X,probbinom,pch=16,cex=1,col="black")
par(new=TRUE)
rn <- rnorm(n=1e6,mean=m,sd=dp)
plot(density(rn), xlab="X", ylab="densidade & probabilidade",
     main=paste0("Distribuicao normal(",m,", ",round(dp,2),")"),
     xlim=c(-1,n+1), ylim=c(0,1.1*max(c(max(probbinom),max(probnorm)))))
dev.off()

