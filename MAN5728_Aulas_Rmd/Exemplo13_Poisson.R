sink("Exemplo13_Poisson.txt")
pdf("Exemplo13_Poisson.pdf")
lambda <- (500000/100000)*(56.2/12)
cat("lambda = ",lambda," casos novos / 500 mil mulheres / mes\n",sep="")
m <- lambda
dp <- sqrt(lambda)
X <- 0:(m+5*dp)
cat("Casos ~ Poisson(lambda = ", round(lambda,2),")\n",sep="")
cat("Media de casos = ",round(m,2),"\n")
cat("Desvio-padrao de casos = ",round(dp,2),"\n",sep="")
probs <- dpois(x=X,lambda=lambda)
probsacum <- ppois(q=X,lambda=lambda)
tbl <- data.frame(X,probs,probsacum)
rownames(tbl) <- X
demanda <- 0.95
leitosdemanda <- max(which(tbl$probsacum<=demanda))
cat("P(casos <= ",leitosdemanda, ") = ",sum(probs[X<=leitosdemanda]),"\n",sep="")
cat("Numero minimo de leitos que satisfaz ",demanda*100, 
    "% da demanda mensal = ", leitosdemanda,"\n",sep="")
round(tbl[,2:3],6)

# Grafico
  leito <- -1
  leitodemanda <- NA
  probabilidade <- 0
  leitos <- c()
  probs <- c()
  while (round(probabilidade,4) < 1)
  {
    leito <- leito+1
    probabilidade <- sum(dpois(x=0:leito, lambda = lambda))
    if (is.na(leitodemanda))
    {
      if (probabilidade>demanda)
      {
        leitodemanda <- leito
      }
    }
    leitos <- c(leitos,leito)
    probs <- c(probs,probabilidade)
  }
  plot (leitos,probs,
        main="Probabilidade acumulada (Poisson)",
        xlab="Leitos", ylab="Demanda atendida",
        ylim=c(0,1), lwd=3, col="#994F88", type = "s",
        axes=FALSE)
  axis(1, at=0:max(leitos))
  axis(2)
  points(leitos,probs,
         col="#994F88", bg="#994F88", pch=21)
  abline(h=demanda, lty=2)
  abline(v=leitodemanda, lty=2)
dev.off()
sink()
