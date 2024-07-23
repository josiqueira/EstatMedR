Dados <- readxl::read_excel("Adm2008.xlsx")
 
# codifica mulher==1, homen==2
Dados$GeneroNum <- NA
generos <- sort(unique(Dados$Genero))
for (g in 1:length(generos))
{
  Dados$GeneroNum[Dados$Genero==generos[g]] <- g
}

cat("Correlacao da Estatura com MCT:")
rXY <- cor(Dados$Estatura,Dados$MCT)
cat(rXY," (r^2=",rXY^2,")\n",sep="")

cat("Correlacao da Estatura com Genero:")
rXZ1 <- cor(Dados$Estatura,Dados$GeneroNum)
cat(rXZ1," (r^2=",rXZ1^2,")\n",sep="")
cat("Correlacao da Estatura com Idade:")
rXZ2 <- cor(Dados$Estatura,Dados$Idade)
cat(rXZ2," (r^2=",rXZ2^2,")\n",sep="")
cat("Correlacao da MCT com Genero:",sep="")
rYZ1 <- cor(Dados$MCT,Dados$GeneroNum)
cat(rYZ1," (r^2=",rYZ1^2,")\n",sep="")
cat("Correlacao da MCT com Idade:",sep="")
rYZ2 <- cor(Dados$MCT,Dados$Idade)
cat(rYZ2," (r^2=",rYZ2^2,")\n",sep="")

cat("Correlacao do Genero com Idade:")
rZ1Z2 <- cor(Dados$GeneroNum,Dados$Idade)
cat(rZ1Z2," (r^2=",rZ1Z2^2,")\n",sep="")

cat("Teste de correlacao parcial\n",
    "(controlando pelo efeito de genero e idade):\n")
rXY.Z1Z2 <- ppcor::pcor.test(x=Dados$Estatura,
                          y=Dados$MCT,
                          z=c(Dados$GeneroNum,Dados$Idade), 
                          method = "pearson")
print(rXY.Z1Z2)
cat("... rXY.Z1Z2^2=",as.numeric(rXY.Z1Z2[1])^2,"\n",sep="")
