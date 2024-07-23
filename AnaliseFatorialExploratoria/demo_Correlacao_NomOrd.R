source("eiras.showdataframe.R")

Dados <- readRDS("Felicidade.rds")
showdataframe(Dados, head=4, tail=3)

# todas as colunas tratadas como intervalares
Dados[,] <- lapply(Dados[,],as.numeric)

# Alterando tipos de variável em algumas colunas
Dados$Q1 <- as.factor(Dados$Q1) # variavel nominal
Dados$Q2 <- as.factor(Dados$Q2) # variavel nominal
Dados$Q3 <- as.ordered(Dados$Q3) # variavel ordinal
Dados$Q4 <- as.ordered(Dados$Q4) # variavel ordinal
# confere os tipos
print(sapply(Dados,class))
showdataframe(Dados, head=4, tail=3)

# matriz de correlacoes
print(polycor::hetcor(Dados, use="pairwise.complete.obs"), digits=1)
