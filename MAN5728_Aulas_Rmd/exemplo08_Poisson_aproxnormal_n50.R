sink("exemplo08_Poisson_aproxnormal_n50.txt")
lambda <- 50
m <- lambda
dp <- sqrt(lambda)
X <- 0:(m+3*dp)
cat("X ~ Poisson(lambda = ", lambda,")\n", sep="")
cat("Media de X = ",m,"\n", sep="")
cat("Desvio-padrao de X = ",round(dp,2),"\n\n", sep="")
if (lambda > 10)
{cat("Poisson(",lambda,") semelhante a normal(",m,", ",round(dp,2),")\n\n", sep="")} else
{cat("Poisson(",lambda,") diferente da normal(",m,", ",round(dp,2),")\n\n", sep="")}
probPois <- dpois(x=X,lambda=lambda)
probnorm <- dnorm(x=X,mean=m,sd=dp)
print(round((tbl <- data.frame(X,probPois,probnorm)),6))
sink()
png("exemplo08_Poisson_aproxnormal_n50.png")
plot(X,probPois,type="h",
     xlim=c(-1,m+3*dp),ylim=c(0,1.1*max(probPois)), 
     xlab="X", ylab="densidade & probabilidade",
     main=paste0("Distribuicao de Poisson(",lambda,")\n\n"),
     lwd=1,col="black")
points(X,probPois,pch=16,cex=1,col="black")
par(new=TRUE)
rn <- rnorm(n=1e6,mean=m,sd=dp)
plot(density(rn), 
     xlab="X", ylab="densidade & probabilidade",
     main=paste0("Distribuicao normal(",m,", ",round(dp,2),")"),
     xlim=c(-1,m+3*dp), ylim=c(0,1.1*max(probPois)))
dev.off()

