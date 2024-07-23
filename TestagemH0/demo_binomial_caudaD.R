# criterio desejado
alfa <- 0.05

s <- 15+1
prob.acumulada <- 0
while (prob.acumulada < alfa)
{
  s <- s-1
  prob.acumulada <- sum(dbinom(s:15,15,0.5))
  cat("P[s>=",s,"]=",prob.acumulada,"\n",sep="")
}
s <- s+1
prob.acumulada <- sum(dbinom(s:15,15,0.5))
cat("\nCauda direita com P[s>=",s,"]=",prob.acumulada,"\n",sep="")

