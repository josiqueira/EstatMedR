source("eiras.shape.test.R")

options(warn=-1)

Dados <- readxl::read_excel("Simpatia.xlsx")
print(Dados)
cat("\nmedia(Antes):",mean(Dados$Antes))
cat("\nmedia(Depois):",mean(Dados$Depois))
dif <- mean(Dados$Depois) - mean(Dados$Antes)
cat("\nDiferenca das medias (Depois-Antes) = ",dif,"\n",sep="")

v <- Dados$Depois - Dados$Antes
# teste de assimetria e normalidade das diferencas
s <- list()
s[["difs"]] <- v
shape <- shape.test(s, labels = c("Diferencas"))

x <- seq(-4,7,length.out=100)
plot(density(v,na.rm=TRUE), xlim=c(-4,7), ylim=c(0,0.25),
     main="Distribuicao, diferenças Depois-Antes",xlab="Score")
lines(x,dnorm(x,mean(v),sd(v)),lty=2)
text(-4,0.2,pos=4,
     paste0("Simmetry: p=",
            round(as.numeric(shape[["simmetry.Diferencas"]]$p.value),4),
            "\nNormality: p=",
            round(as.numeric(shape[["normality.Diferencas"]]$p.value),4),
            "\nn=",length(v)
     )
)

cat(eiras::BarTitle("Teste t"))
print(t.test(v))

options(warn=0)
