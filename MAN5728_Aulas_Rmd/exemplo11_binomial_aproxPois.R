sink("exemplo11_binomial_aproxPois.txt")
n <- 500
p <- 1/1000
cat("X ~ binomial(",n,";",p,")\n")
m <- n*p
dp <- sqrt(n*p*(1-p))
cat("Media de X =",m,"\n")
cat("Desvio-padrao de X =",dp,"\n")
X <- 0:n
if (n >= 20 && p <= 0.05)
{cat("Binomial(",n,";",p,") semelhante a Poisson(",m,")\n")} else
{cat("Binomial(",n,";",p,") diferente de Poisson(",m,")\n")}
probbinom <- dbinom(x=X,size=n,prob=p)
probbinomacum <- pbinom(q=X,size=n,prob=p)
probPois <- dpois(x=X,lambda=m)
tbl <- data.frame(X,probbinom,probbinomacum,probPois)
round(tbl[tbl$probbinomacum<=0.999,],6)
sink()
pdf("exemplo11_binomial_aproxPois.pdf")
plot(X,probbinom,type="h",
     main=paste("Distribuicao binomial(",n,";",p,"): circulo\n\n"),
     xlab="X", ylab="probabilidade",
     xlim=c(-1,m+5*dp), ylim=c(0,1.1*max(c(max(probbinom),max(probPois)))),
     lwd=1,col="black")
points(X,probbinom,pch=16,cex=1,col="black")
par(new=TRUE)
plot(X,probPois,type="h",
     main=paste("Distribuicao de Poisson(",m,"): quadrado"),
     xlab="X", ylab="probabilidade",
     xlim=c(-1,m+5*dp), ylim=c(0,1.1*max(c(max(probbinom),max(probPois)))),
     lwd=1,col="black")
points(X,probPois,pch=12,cex=1,col="black")
dev.off()

