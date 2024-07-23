jogadas <- 5
sucesso <- 0:jogadas
probabilidade <- dbinom(sucesso,jogadas,0.5)
binomial <- data.frame(sucesso,probabilidade)
names(binomial) <- c("Sucesso","FR")
print(binomial)
