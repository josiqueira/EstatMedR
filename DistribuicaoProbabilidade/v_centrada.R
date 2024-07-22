v.centrada <- scale(v, center=TRUE, scale=FALSE)
d <- density(v.centrada)
plot (d, type="l", 
      main="Distribuição fictícia centrada",
      xlim=c(-4,4),
      xlab="Creatinina", 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(v.centrada),6),"\n", sep="")
cat("  dp = ", round(sd(v.centrada),6),"\n", sep="")