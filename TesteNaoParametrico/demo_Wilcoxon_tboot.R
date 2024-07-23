options(warn=-1)
alfa=0.05

Dados <- readxl::read_excel("Simpatia.xlsx")
print(Dados)
cat("\nmedia(Antes):",mean(Dados$Antes))
cat("\nmedia(Depois):",mean(Dados$Depois))
dif <- mean(Dados$Depois) - mean(Dados$Antes)
cat("\nDiferenca das medias (Depois-Antes) = ",dif,"\n",sep="")

# t test
# amostra unica
v <- Dados$Depois - Dados$Antes
n <- length(v) # tamanho da amostra
m <- mean(v) # media amostral
s <- sd(v) # desvio-padrao amostral
ep <- s/sqrt(n) # erro-padrao
B <- 1e5
Tstar <- rep(0,B)
for (i in 1:B)
{
  x <- sample(v, n, replace=TRUE)
  Tstar[i] <- (mean(x)-m)/(sd(x)/sqrt(n))
}
ICpv <- m + quantile(Tstar,c(alfa/2,0.5,1-alfa/2))*ep
cat(paste0("\nOne-sample t test (",B," replicates)\n"))
print(ICpv)

options(warn=0)
