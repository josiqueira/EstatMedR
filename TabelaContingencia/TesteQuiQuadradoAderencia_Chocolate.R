# TesteQuiQuadradoAderencia_Chocolate.R
invisible(Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8"))
invisible(Sys.setlocale("LC_ALL", "pt_BR.UTF-8"))
# carrega a função chisqgof()
source("eiras.chisqgof.R")

# arquivo de dados no formato _rds_ com os dados e hipótese.
# primeira coluna é o fator
# segunda coluna com frequências observadas
# coluna **probH0** contém 1/3 em todas as linhas (frequências esperadas para a hipótese nula)
Dados <- readRDS("Chocolate.rds")
# executa o teste
fit <- chisqgof(Dados)
