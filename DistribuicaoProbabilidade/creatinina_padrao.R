v.padrao <- scale(v, center=TRUE, scale=TRUE)
d <- density(v.padrao)
plot (d, type="l", 
      main="Distribuição fictícia padronizada",
      xlim=c(-4,4),
      xlab="Creatinina", 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(v.padrao),6),"\n", sep="")
cat("  dp = ", round(sd(v.padrao),6),"\n", sep="")