mp <- 175 # media populacional
dpp <- 8 # desvio-padrao populacional
if(is.na(B)){B <- 1e3}

cat ("Média populacional=",mp,"\n",sep="")
cat ("Desvio padrão populacional=",dpp,"\n",sep="")

n <- 30 # tamanho da amostra
cat("n =", n, "\n")
alfa <- 0.05 # nivel de significancia

plot(x = c(mp-dpp, mp+dpp), y = c(1, 100), type = "n", 
     xlab = paste("Estatura masculina (média=",mp," cm)",sep=""), 
     ylab = "",
     axes=FALSE)
abline(v = mp, col = "red") # linha vertical da media populacional

# sampling
counter <- 0 # set counter to 0
for (i in 1:B)
{
  x <- rnorm(n, mp, dpp) # amostra aleatoria normal de tamanho n
  s <- sd(x) 
  L <- mean(x) - qt(1-alfa/2,n-1)*s/sqrt(n) # lower limit
  U <- mean(x) + qt(1-alfa/2,n-1)*s/sqrt(n) # upper limit
  if (L < mp && mp < U) # verifica se a media pop esta contida em IC95
  {
    color <- "#222222" # cinza
    counter <- counter + 1 # Se sim, soma 1 no contador
    espessura <- 0.5
  } else
  {
    color <- "#1965B0" # azul cobalto
    espessura <- 2
  }
  if (i <= 100) # plota os primeiros IC95%
    segments(L, i, U, i, col=color, lwd=espessura)
}
pIC95 <- counter/B # % de IC95 que contem a media populacional.
cat("\nProporcao dos ",
    format(B, scientific = FALSE),
    " IC95(media pop)\n",
    "que contem a media pop ",
    mp, " = ",
    round(pIC95,5), sep="")

