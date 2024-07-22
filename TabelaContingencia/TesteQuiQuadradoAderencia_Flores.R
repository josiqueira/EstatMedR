# TesteQuiQuadradoAderencia_Flores.R

library(readxl)
source ("eiras.chisqgof.R")

# pega o arquivo com os dados
df_flores <- as.data.frame(readxl::read_excel("flores.xlsx"))

# chama os testes estatísticos (duas vezes)
cat("\n### Hipotese de codominancia: ###\n")
chisqgof(df_flores[,c(1,2,3)])
cat("\n### Hipotese de epistasis recessiva: ###\n")
chisqgof(df_flores[,c(1,2,4)])
