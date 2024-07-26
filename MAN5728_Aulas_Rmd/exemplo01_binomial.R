sink("exemplo01_binomial.txt") 
n <- 3
p <- 0.15
cat("X ~ binomial (",n,", ",p,")\n",sep="")
m <- n*p
dp <- sqrt(n*p*(1-p))
cat("Media de X =",m,"\n")
cat("Desvio-padrao de X =",dp,"\n\n")
px0 <- dbinom(x=0,size=n,prob=p)
px1 <- dbinom(x=1,size=n,prob=p)
px2 <- dbinom(x=2,size=n,prob=p)
px3 <- dbinom(x=3,size=n,prob=p)
cat("P(X = 0) =",px0,"\n")
cat("P(X = 1) =",px1,"\n")
cat("P(X = 2) =",px2,"\n")
cat("P(X = 3) =",px3,"\n")
cat("\nP(X = 0) + P(X = 1) + P(X = 2) + P(X = 3) =", 
    px0+px1+px2+px3, "\n")
# lower.tail: logical; if TRUE (default), probabilities are P[X <= x], 
#                      otherwise, P[X > x]
px01 <- pbinom(q=1,size=n,prob=p,lower.tail=TRUE)
cat("P(X = 0) + P(X = 1) =", px01, "\n")
px23 <- pbinom(q=1,size=n,prob=p,lower.tail=FALSE)
cat("P(X = 2) + P(X = 3) =", px23, "\n")
sink()
png("exemplo01_binomial.png")
X <- 0:n
probs <- dbinom(x=X,size=n,prob=p)
plot(X,
     probs,
     type="h",
     xlim=c(-1,n+1),
     ylim=c(0,1.1*max(probs)), 
     main=paste0("Distribuicao binomial (",n,", ",p,")"),
     lwd=1,
     col="black",
     ylab="Probabilidade")
points(X,probs,pch=16,cex=1,col="black")
dev.off()
