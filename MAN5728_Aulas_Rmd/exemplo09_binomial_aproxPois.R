sink("exemplo09_binomial_aproxPois_n10p10.txt")
n <- 20
p <- 0.2
cat("X ~ binomial(",n,", ",p,")\n", sep="")
m <- n*p
dp <- sqrt(n*p*(1-p))
cat("Media de X = ",m,"\n", sep="")
cat("Desvio-padrao de X = ",round(dp,2),"\n", sep="")
X <- 0:n
if (n >= 20 && p <= 0.05)
{cat("Binomial(",n,", ",p,") semelhante a Poisson(",m,")\n\n", sep="")} else
{cat("Binomial(",n,", ",p,") diferente de Poisson(",m,")\n\n", sep="")}
probbinom <- dbinom(x=X,size=n,prob=p)
probPois <- dpois(x=X,lambda=m)
print(round((tbl <- data.frame(X,probbinom,probPois)),6))
sink()
png("exemplo09_binomial_aproxPois.png")
plot(X,probbinom,type="h",
     main=paste0("Distribuicao binomial(",n,", ",p,"): circulo\n\n"),
     xlab="X", ylab="probabilidade",
     xlim=c(-1,m+5*dp), ylim=c(0,1.1*max(c(max(probbinom),max(probPois)))),
     lwd=1,col="black")
points(X,probbinom,pch=16,cex=1,col="black")
par(new=TRUE)
plot(X,probPois,type="h",
     main=paste0("Distribuicao de Poisson(",m,"): quadrado"),
     xlab="X", ylab="probabilidade",
     xlim=c(-1,m+5*dp), ylim=c(0,1.1*max(c(max(probbinom),max(probPois)))),
     lwd=1,col="black")
points(X,probPois,pch=12,cex=1,col="black")
dev.off()

