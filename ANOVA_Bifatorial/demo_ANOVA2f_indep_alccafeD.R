# demo_ANOVA2f_indep.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(psych))
suppressMessages(library(lattice))
suppressMessages(library(RcmdrMisc))
suppressMessages(library(gplots))
suppressMessages(library(car))

suppressMessages(library(lawstat))

suppressMessages(library(MuMIn))
suppressMessages(library(heplots))

suppressMessages(library(phia))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

Dados <- read_excel("CafeinaAlcool_entreD.xlsx")
Dados$Cafeina <- factor(Dados$Cafeina)
Dados$Alcool <- factor(Dados$Alcool)
Dados$UE <- factor(Dados$UE)
n.f1 <- length(unique(Dados$Cafeina))
n.f2 <- length(unique(Dados$Alcool))

cat(bartitle("Data"))
showdataframe(Dados)
cat("\n\tFactors (Cafeina & Alcool):\n")
print(table(Dados$Cafeina, Dados$Alcool))

cat(bartitle("Descriptive Statistics"))

cat(bartitle("NumErros",2))
cat(bartitle("Estratification by Alcool",3))
print(with(Dados, psych::describeBy(x=NumErros,group=list(Alcool),digits=2)))
cat(bartitle("Estratification by Cafeina",3))
print(with(Dados, psych::describeBy(x=NumErros,group=list(Cafeina),digits=2)))
cat(bartitle("Estratification by Alcool and Cafeina",3))
print(with(Dados, psych::describeBy(x=NumErros,group=list(Cafeina,Alcool),digits=2)))

boxplot(NumErros~Cafeina * Alcool, data=Dados,
        ylab="NumErros")

print(grf <- lattice::xyplot(NumErros~Cafeina, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(NumErros~Alcool, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(NumErros~Cafeina:Alcool, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(NumErros~Cafeina|Alcool, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))

with(Dados, gplots::plotmeans(NumErros~Cafeina,
                              error.bars="conf.int", level=.95,
                              connect=FALSE,
                              ylab=names(Dados)[which(names(Dados)=="NumErros")],
                              xlab=names(Dados)[which(names(Dados)=="Cafeina")],
                              main="IC95%",
                              barcol="black"))
with(Dados, gplots::plotmeans(NumErros~Alcool,
                              error.bars="conf.int", level=.95,
                              connect=FALSE,
                              ylab=names(Dados)[which(names(Dados)=="NumErros")],
                              xlab=names(Dados)[which(names(Dados)=="Alcool")],
                              main="IC95%",
                              barcol="black"))

with(Dados, 
     RcmdrMisc::plotMeans(NumErros, Cafeina, Alcool,
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="Cafeina", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))
with(Dados, 
     RcmdrMisc::plotMeans(NumErros, Alcool, Cafeina, 
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="Alcool", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))

car::densityPlot(NumErros~Cafeina, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f1))
car::densityPlot(NumErros~Alcool, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f2))

plot.design(NumErros~Cafeina, data=Dados)
plot.design(NumErros~Alcool, data=Dados)
plot.design(NumErros~Cafeina + Alcool + Cafeina:Alcool, data=Dados)

cat(bartitle("Assumptions"))
cat(bartitle("Symmetry",2))
cat(bartitle("by Alcool",3))
Alcool <- unique(Dados$Alcool)
for (i in 1:length(Alcool))
{
  cat("\nAlcool = ",as.character(Alcool[i]),"\n")
  print(lawstat::symmetry.test(Dados$NumErros[Dados$Alcool==Alcool[i]]))
}
cat(bartitle("by Cafeina",3))
Cafeina <- unique(Dados$Cafeina)
for (i in 1:length(Cafeina))
{
  cat("\nCafeina = ",as.character(Cafeina[i]),"\n")
  print(lawstat::symmetry.test(Dados$NumErros[Dados$Cafeina==Cafeina[i]]))
}

cat(bartitle("Normality",2))
cat(bartitle("by Alcool",3))
print(RcmdrMisc::normalityTest(NumErros ~ Alcool, data=Dados))
cat(bartitle("by Cafeina",3))
print(RcmdrMisc::normalityTest(NumErros ~ Cafeina, data=Dados))

cat(bartitle("Homoscedasticity",2))
cat(bartitle("by Alcool",3))
print(car::leveneTest(NumErros ~ Alcool, data=Dados, center=median))
cat(bartitle("by Cafeina",3))
print(car::leveneTest(NumErros ~ Cafeina, data=Dados, center=median))


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

# ANOVA da ANOVA
cat(bartitle("ANOVA",2))
print(anv <- car::Anova(modelo, type=2, white.adjust=TRUE))


cat(bartitle("Effect size analysis"))
cat(bartitle("Omnibus",2))
eta2 <- as.numeric(MuMIn::r.squaredLR(modelo))
Eta2classification(eta2)
cat(bartitle("Partials",2))
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


cat(bartitle("Simple Main Effects"))

cat("\n- Alcool at each level of Cafeina:\n")
print(phia::testInteractions(modelo, fixed="Cafeina", across="Alcool"))

cat("\n- Cafeina at each level of Alcool:\n")
print(phia::testInteractions(modelo, fixed="Alcool", across="Cafeina"))

# Grafico
fit.means <- phia::interactionMeans(modelo)
plot(fit.means, errorbar=paste("ci",round((1-alfa)*100,0),sep=""))

# enable warnings
options(warn=0)
