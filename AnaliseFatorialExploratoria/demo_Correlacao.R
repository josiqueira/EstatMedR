Dados <- readRDS("Felicidade.rds")
print(head(Dados))
print(tail(Dados))

# todas as colunas tratadas como intervalares
Dados[,] <- lapply(Dados[,],as.numeric)

# Correlacoes heterogeneas
print(polycor::hetcor(Dados, use="pairwise.complete.obs"), digits=1)


