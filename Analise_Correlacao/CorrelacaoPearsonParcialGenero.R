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
rXZ <- cor(Dados$Estatura,Dados$GeneroNum)
cat(rXZ," (r^2=",rXZ^2,")\n",sep="")
cat("Correlacao da MCT com Genero:")
rYZ <- cor(Dados$MCT,Dados$GeneroNum)
cat(rYZ," (r^2=",rYZ^2,")\n",sep="")

cat("Teste de correlacao parcial\n",
    "(controlando pelo efeito do genero):\n")
rXY.Z <- ppcor::pcor.test(x=Dados$Estatura,
                         y=Dados$MCT,
                         z=Dados$GeneroNum, 
                         method = "pearson")
print(rXY.Z)
cat("... rXY.Z^2=",as.numeric(rXY.Z[1])^2,"\n",sep="")
