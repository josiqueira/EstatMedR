jogadas <- 5
sucesso <- 0:jogadas
probabilidade <- dbinom(sucesso,jogadas,0.5)
cat ("Sucesso\tProbabilidade\n")
for (i in 1:(jogadas+1))
{
  cat (sucesso[i],"\t",probabilidade[i],"\n")
}
