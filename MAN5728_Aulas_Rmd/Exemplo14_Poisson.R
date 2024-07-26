sink("exemplo14_Poisson.txt")
lambda <- 1000/365
m <- lambda
dp <- sqrt(lambda)
X <- 0:(m+5*dp)
cat("X ~ Poisson(lambda =", round(lambda,2),")\n")
cat("Media de X =",round(m,2),"\n")
cat("Desvio-padrao de X =",round(dp,2),"\n")
probs <- dpois(x=X,lambda=lambda)
cat("P(X>=5) =",sum(probs[X>=5]),"\n")
probsacum <- ppois(q=X,lambda=lambda)
tbl <- data.frame(X,probs,probsacum)
npartos95 <- max(which(tbl$probsacum<=.95))
cat("Numero de partos que satisfaz pelo menos 95% da demanda no periodo =",
    npartos95,"\n")
npartos99 <- max(which(tbl$probsacum<=.99))
cat("Numero minimo de leitos que satisfaz pelo menos 99% da demanda no periodo =",
    npartos99,"\n")
round(tbl,6)
sink()
pdf("exemplo14_Poisson.pdf")
plot(X,probs,type="h",
     xlim=c(-1,(m+5*dp)),ylim=c(0,1.05*max(probs)), 
     main=paste("Distribuicao de Poisson(",round(lambda,2),")"),
     lwd=1,col="black",ylab="Probabilidade",xlab="#partos/periodo0-8h")
points(X,probs,pch=16,cex=1,col="black")
dev.off()
