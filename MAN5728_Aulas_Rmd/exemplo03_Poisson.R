sink("exemplo03_Poisson.txt")
lambda <- 2.3
m <- lambda
dp <- sqrt(lambda)
X <- 0:(m+5*dp)
cat("X ~ Poisson(lambda =", lambda,")\n")
cat("Media de X =",m,"\n")
cat("Desvio-padrao de X =",dp,"\n\n")
probs <- dpois(x=X,lambda=lambda)
probsacum <- ppois(q=X,lambda=lambda)
print(tbl <- round(data.frame(X,probs,probsacum),6))
cat("\nP(X=0) + P(X=1) + ... + P(X=10) =",sum(probs),"\n")
# lower.tail: logical; if TRUE (default), probabilities are P[X <= x], 
#                      otherwise, P[X > x]
px01 <- ppois(q=1,lambda=lambda,lower.tail=TRUE)
cat("P(X=0) + P(X=1) =", px01, "\n")
px23inf <- ppois(q=1,lambda=lambda,lower.tail=FALSE)
cat("P(X=2) + P(X=3) + ... =", px23inf, "\n")
sink()
png("exemplo03_Poisson.png")
plot(X,probs,type="h",
     xlim=c(-1,(m+5*dp)),ylim=c(0,1.1*max(probs)), 
     main=paste("Distribuicao de Poisson(",lambda,")"),
     lwd=1,col="black",ylab="Probabilidade")
points(X,probs,pch=16,cex=1,col="black")
dev.off()