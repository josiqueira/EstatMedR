nomelin<-"H"
nomecol<-"M"
a<-50
b<-150
c<-125
d<-175
tabela <- data.frame(
  c1 = c("", paste(nomelin,"+"), paste(nomelin,"-"), "" ),
  c2 = c(paste(nomecol,"+"), round(a,3), round(c,3), round((a+c),3)),
  c3 = c(paste(nomecol,"-"), round(b,3), round(d,3), round((b+d),3)),
  c4 = c("", round(a+b,3), round(c+d,3), round(a+b+c+d,3))
)
names(tabela) <- NULL
print(tabela, row.names = FALSE)
