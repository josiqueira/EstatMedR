nomelin<-"T"
nomecol<-"D"
tabela <- matrix(data=c(
  "a", "b", "a+b",
  "c", "d", "c+d",
  "a+c", "b+d", "total"
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
