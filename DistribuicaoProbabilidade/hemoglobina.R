# para reproduzir o exemplo
set.seed(837)
# numero de individuos
n <- 100
# media
mu <- 13.5 
# desvio-padrao
dp <-1.5 
# unidade
txtun <- "mg/dl"
# cria a populacao e exibe o grafico
v <- rnorm(n,mu,dp)
d <- density(v)
plot (d, type="l", 
      main="Distribuicao ficticia",
      xlab=paste("Hemoglobina (",txtun,")",sep=""), 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(v),6),"\n", sep="")
cat("  dp = ", round(sd(v),6),"\n", sep="")
