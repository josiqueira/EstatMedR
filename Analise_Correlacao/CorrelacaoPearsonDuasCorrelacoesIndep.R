Dados <- readxl::read_excel("Adm2008.xlsx")
DadosF <- Dados[Dados$Genero=="Feminino",]
DadosM <- Dados[Dados$Genero=="Masculino",]
nF <- nrow(DadosF)
nM <- nrow(DadosM)

# saida textual 
source("eiras.bartitle.R")

cat(bartitle("Estatística descritiva"))
cat(bartitle("- sexo feminino"))
print(summary(Dados[Dados$Genero=="Feminino",3:4]))
cat("n: ",nF,"\n",sep="")
cat("Correlacao: ",cor(Dados[Dados$Genero=="Feminino",3:4])[1,2],"\n",sep="")
cat(bartitle("- sexo masculino"))
print(summary(Dados[Dados$Genero=="Masculino",3:4]))
cat("n: ",nM,"\n",sep="")
cat("Correlacao: ",cor(Dados[Dados$Genero=="Masculino",3:4])[1,2],"\n",sep="")

# Elipse de predicao 95%
matriz <- as.matrix(Dados[, 3:4])
n <- nrow(matriz)
car::dataEllipse(matriz[,1], matriz[,2],
                 groups=factor(Dados$Genero),
                 group.labels=c("F", "M"),
                 levels=c(.95,.999),
                 robust=TRUE,
                 main=paste("Elipses de predicao de 95% e 99.9%\n",
                            "n = ",n," (Fem.=",nF,", Masc.=",nM,")",sep=""),
                 xlab="Estatura (m)",
                 ylab="MCT (kg)",
                 xlim=c(1.3, 2.1),
                 ylim=c(25, 120),
                 lwd=0.8, lty=2)

