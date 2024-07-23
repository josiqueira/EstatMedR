source("eiras.showdataframe.R")

Dados <- readRDS("Felicidade.rds")
showdataframe(Dados, head=4, tail=3)

# todas as colunas tratadas como intervalares
Dados[,] <- lapply(Dados[,],as.numeric)

# Correlacoes heterogeneas
r <- polycor::hetcor(Dados, use="pairwise.complete.obs")

print(
knitr::kable(r$correlations,
             digits=2, 
             booktabs=TRUE, 
             format="markdown")
)


