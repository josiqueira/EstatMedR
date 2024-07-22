nomelin<-"H"
nomecol<-"M"
a<-50
b<-150
c<-125
d<-175
tabela <- matrix(data=c(a,b,c,d), ncol=2, byrow = TRUE)
colnames(tabela) <- c(paste(nomecol,"+",sep=""),paste(nomecol,"-",sep=""))
rownames(tabela) <- c(paste(nomelin,"+",sep=""),paste(nomelin,"-",sep=""))
prmatrix(tabela)
