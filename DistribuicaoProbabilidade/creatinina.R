# semente, para repetir este exemplo
set.seed(6092)
# numero de individuos
n <- 100
# assumindo que a distribuicao eh simetrica%
mu <- 0.8 # (0.5+1.1)/2
# assumindo que deram o intervalo de 95%
dp <- 0.15 # (1.1-0.5)/4
# cria a populacao e exibe o grafico
v <- rnorm(n,mu,dp)
d <- density(v)
plot (d, type="l", 
      main="Distribuição fictícia",
      xlim=c(mu-4*dp,mu+4*dp),
      xlab="Creatinina", 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(v.centrada),6),"\n", sep="")
cat("  dp = ", round(sd(v.centrada),6),"\n", sep="")