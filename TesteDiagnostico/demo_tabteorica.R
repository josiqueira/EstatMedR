nomelin<-"T"
nomecol<-"D"
tabela <- matrix(data=c(
  "Exame acertou", "Falso positivo", "Total de positivos",
  "Falso negativo", "Exame acertou", "Total de negativos",
  "Total de doentes", "Total de não doentes", ""
), ncol=3, byrow = TRUE)
colnames(tabela) <- c(
  paste(nomecol,"+",sep=""),
  paste(nomecol,"-",sep=""),
  ""
)
rownames(tabela) <- c(
  paste(nomelin,"+",sep=""),
  paste(nomelin,"-",sep=""),
  ""
)
prmatrix(tabela, quote=FALSE)
