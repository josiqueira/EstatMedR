nomelin<-"Linha"
nomecol<-"Coluna"
tabela <- data.frame(
  c1 = c("", paste(nomelin,"+",sep=""), paste(nomelin,"-",sep=""), "" ),
  c2 = c(paste(nomecol,"+",sep=""), "a", "c", "a+c"),
  c3 = c(paste(nomecol,"-",sep=""), "b", "d", "b+d"),
  c4 = c("", "a+b", "c+d", "a+b+c+d")
)
names(tabela) <- NULL
print(tabela, row.names = FALSE)
