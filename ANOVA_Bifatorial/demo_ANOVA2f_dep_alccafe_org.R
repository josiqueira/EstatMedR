# demo_ANOVA2f_indep.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(MuMIn))
suppressMessages(library(heplots))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

Dados <- read_excel("CafeinaAlcool_intra.xlsx")
Dados$Cafeina <- factor(Dados$Cafeina)
Dados$Alcool <- factor(Dados$Alcool)
Dados$UE <- factor(Dados$UE)

cat(bartitle("Data"))
showdataframe(Dados,4,3)

cat(bartitle("ANOVA relacionada bifatorial"))

alfa <- 0.05
cat(bartitle("Analise de significancia estatistica: teste omnibus"))
modelo <- lm(NumErros~Cafeina*Alcool + UE, data=Dados)
print(reg <- summary(modelo))
print(anv <- car::Anova(modelo, type=2, white.adjust=FALSE))

# grafico
fobs <- reg$fstatistic[1]
dfn <- reg$fstatistic[2]
dfd <- reg$fstatistic[3]
fc <- qf(1-alfa, dfn, dfd)
p <- 1-pf(fobs, dfn, dfd)
if (p < 1e-4)
{
  p <- sprintf("%.2e",p)
} else
{
  p <- sprintf("%.4f",p)
}
f <- seq(0,1.4*max(fc,fobs),length.out=300)
densf <- df(f, dfn, dfd)
plot(f, densf, 
     main="Omnibus test",
     xlab="F", ylab="Density", 
     lwd=2, type="l")
abline(v=fc, lty=3)
abline(v=fobs, lty=4)
legend("topright",
       c("H0: there is not model",
         paste("Fc(",dfn,",",dfd,") = ",round(fc,3),sep=""),
         paste("Fobs = ",round(fobs,3),"\n",
               "p = ",p,sep="")
       ),
       lwd=c(2,1,1), lty=c(1,3,4),
       cex=0.8, 
       box.lwd=0, bg="transparent")


cat(bartitle("Analise de significancia pratica: tamanho de efeito"))
cat("\n- Omnibus:\n")
eta2 <- as.numeric(MuMIn::r.squaredLR(modelo))
Eta2classification(eta2)
cat("\n- Partial(s):\n")
eta2p <- heplots::etasq(modelo)
eta2p$classification <- NA
for (r in 1:nrow(eta2p))
{
  if(!is.na(eta2p$`Partial eta^2`[r]))
  {
    eta2p$classification[r] <- Eta2classification(eta2p$`Partial eta^2`[r],show=FALSE)  
  }
}
eta2p <- eta2p[!is.na(eta2p$`Partial eta^2`),]
prmatrix(eta2p,quote=FALSE)

# enable warnings
options(warn=0)
