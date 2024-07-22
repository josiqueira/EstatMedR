# trazendo a funcao numeric.summary() que desenvolvemos
source("eiras.numeric.summary.R")

set.seed(3972) # para repetir este exemplo

# gera valores com distribuicao normal
valores <- rnorm(1e6, mean=35, sd=12)

# calcula media e desvio-padrao
media <- mean(valores)
desvpad <- sd(valores)

# prepara o titulo do grafico
titulo <- paste("Distribuição normal","\n",
                "(média=", round(media,2),", ",
                "dp=",round(desvpad,2),")",
                sep="")
# exibe o grafico
densidade <- density(valores)
plot (densidade, 
      main=titulo, 
      xlab="Valores", ylab="Densidade",
      lwd=3, type="l")

# exibe os valores calculados
# media e dp
cat("Média simulada = ", media,"\n", sep="")
cat("Dp simulado = ", desvpad,"\n", sep="")
# sumario
cat("\nFunção do R - summary():\n")
print(summary(valores))
# funcao propria
cat("\nFunção 'eiras' - numeric.summary():\n")
print(numeric.summary(valores))
