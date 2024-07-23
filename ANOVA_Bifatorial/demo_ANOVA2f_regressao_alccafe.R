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

Dados <- readxl::read_excel("CafeinaAlcool_entre.xlsx")
Dados$Cafeina <- factor(Dados$Cafeina)
Dados$Alcool <- factor(Dados$Alcool)
Dados$UE <- factor(Dados$UE)

cat(bartitle("Data"))
showdataframe(Dados,4,3)

cat(bartitle("Fisher's Bifatorial independent ANOVA\nwith White's heteroscedasticity correction"))

alfa <- 0.05

# Regressao da ANOVA
cat(bartitle("Statistical analysis: omnibus test",2))
modelo <- lm(NumErros ~ Alcool*Cafeina, data=Dados)
print(reg <- summary(modelo))
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


# enable warnings
options(warn=0)
