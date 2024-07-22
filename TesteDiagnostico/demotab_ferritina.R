nomelin<-"Ferritina"
nomecol<-"Anemia"
a<-731
b<-270
c<-78
d<-1500
tabela <- matrix(data=c(a,b,c,d), ncol=2, byrow = TRUE)
colnames(tabela) <- c(paste(nomecol,"+",sep=""),paste(nomecol,"-",sep=""))
rownames(tabela) <- c(paste(nomelin,"+",sep=""),paste(nomelin,"-",sep=""))
prmatrix(tabela)
