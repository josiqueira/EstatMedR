Dados <- readxl::read_excel("Adm2008.xlsx")
# padronizacao
Dados$Estatura.z <- unsplit(lapply(split(Dados$Estatura, Dados$Genero), scale), 
                            Dados$Genero)
Dados$MCT.z <- unsplit(lapply(split(Dados$MCT, Dados$Genero), scale), 
                            Dados$Genero)
nF <- sum(Dados$Genero=="Feminino")
nM <- sum(Dados$Genero=="Masculino")
# saida textual
source("eiras.bartitle.R")
 
# Elipse de predicao 95%
df <- data.frame(Dados$Estatura.z,Dados$MCT.z)

matriz <- as.matrix(df)
n <- nrow(matriz)
car::dataEllipse(matriz[,1], matriz[,2],
                 groups=factor(Dados$Genero),
                 group.labels=c("F", "M"),
                 levels=0.95,
                 robust=TRUE,
                 main=paste("Elipses de predicao de 95%\n",
                            "n = ",n," (Fem.=",nF,", Masc.=",nM,")",sep=""),
                 xlab="Estatura (z)",
                 ylab="MCT (z)",
                 xlim=c(-4,4),
                 ylim=c(-4,4),
                 lwd=0.8, lty=2)

