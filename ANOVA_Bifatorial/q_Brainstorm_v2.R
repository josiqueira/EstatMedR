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

Dados <- read.csv("Brainstorming20_5Kutner.dat", sep="\t")
Dados$Profissional <- factor(Dados$Profissional)
Dados$TamanhoGrupo <- factor(Dados$TamanhoGrupo)
n.f1 <- length(unique(Dados$Profissional))
n.f2 <- length(unique(Dados$TamanhoGrupo))

cat(bartitle("Data"))
showdataframe(Dados)
cat("\n\tFactors (Profissional & TamanhoGrupo):\n")
print(table(Dados$Profissional, Dados$TamanhoGrupo))

cat(bartitle("Descriptive Statistics"))

cat(bartitle("Ideias",2))
cat(bartitle("Estratification by TamanhoGrupo",3))
print(with(Dados, psych::describeBy(x=Ideias,group=list(TamanhoGrupo),digits=2)))
cat(bartitle("Estratification by Profissional",3))
print(with(Dados, psych::describeBy(x=Ideias,group=list(Profissional),digits=2)))
cat(bartitle("Estratification by TamanhoGrupo and Profissional",3))
print(with(Dados, psych::describeBy(x=Ideias,group=list(Profissional,TamanhoGrupo),digits=2)))

boxplot(Ideias~Profissional * TamanhoGrupo, data=Dados,
        ylab="Ideias")

print(grf <- lattice::xyplot(Ideias~Profissional, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(Ideias~TamanhoGrupo, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(Ideias~Profissional:TamanhoGrupo, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(Ideias~Profissional|TamanhoGrupo, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))

with(Dados, gplots::plotmeans(Ideias~Profissional,
                              error.bars="conf.int", level=.95,
                              connect=FALSE,
                              ylab=names(Dados)[which(names(Dados)=="Ideias")],
                              xlab=names(Dados)[which(names(Dados)=="Profissional")],
                              main="IC95%",
                              barcol="black"))
with(Dados, gplots::plotmeans(Ideias~TamanhoGrupo,
                              error.bars="conf.int", level=.95,
                              connect=FALSE,
                              ylab=names(Dados)[which(names(Dados)=="Ideias")],
                              xlab=names(Dados)[which(names(Dados)=="TamanhoGrupo")],
                              main="IC95%",
                              barcol="black"))

with(Dados, 
     RcmdrMisc::plotMeans(Ideias, Profissional, TamanhoGrupo,
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="Profissional", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))
with(Dados, 
     RcmdrMisc::plotMeans(Ideias, TamanhoGrupo, Profissional, 
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="TamanhoGrupo", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))

car::densityPlot(Ideias~Profissional, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f1))
car::densityPlot(Ideias~TamanhoGrupo, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f2))

plot.design(Ideias~Profissional, data=Dados)
plot.design(Ideias~TamanhoGrupo, data=Dados)
plot.design(Ideias~Profissional + TamanhoGrupo + Profissional:TamanhoGrupo, data=Dados)

cat(bartitle("Assumptions"))
cat(bartitle("Symmetry",2))
cat(bartitle("by TamanhoGrupo",3))
TamanhoGrupo <- unique(Dados$TamanhoGrupo)
for (i in 1:length(TamanhoGrupo))
{
  cat("\nTamanhoGrupo = ",as.character(TamanhoGrupo[i]),"\n")
  try(
    print(lawstat::symmetry.test(Dados$Ideias[Dados$TamanhoGrupo==TamanhoGrupo[i]]))
  )
}
cat(bartitle("by Profissional",3))
Profissional <- unique(Dados$Profissional)
for (i in 1:length(Profissional))
{
  cat("\nProfissional = ",as.character(Profissional[i]),"\n")
  try(
    print(lawstat::symmetry.test(Dados$Ideias[Dados$Profissional==Profissional[i]]))
  )    
}

cat(bartitle("Normality",2))
cat(bartitle("by TamanhoGrupo",3))
try(
  print(RcmdrMisc::normalityTest(Ideias ~ TamanhoGrupo, data=Dados))
)
cat(bartitle("by Profissional",3))
try(
  print(RcmdrMisc::normalityTest(Ideias ~ Profissional, data=Dados))
)

cat(bartitle("Homoscedasticity",2))
cat(bartitle("by TamanhoGrupo",3))
try(
  print(car::leveneTest(Ideias ~ TamanhoGrupo, data=Dados, center=median))
)
cat(bartitle("by Profissional",3))
try(
  print(car::leveneTest(Ideias ~ Profissional, data=Dados, center=median))
)

cat(bartitle("Fisher's Bifatorial independent ANOVA\nwith White's heteroscedasticity correction"))

alfa <- 0.05

# Regressao da ANOVA
cat(bartitle("Statistical analysis: omnibus test",2))
modelo <- lm(Ideias ~ TamanhoGrupo+Profissional, data=Dados)
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

cat("\n- TamanhoGrupo at each level of Profissional:\n")
print(phia::testInteractions(modelo, fixed="Profissional", across="TamanhoGrupo"))

cat("\n- Profissional at each level of TamanhoGrupo:\n")
print(phia::testInteractions(modelo, fixed="TamanhoGrupo", across="Profissional"))

# Grafico
o.par <- par()
fit.means <- phia::interactionMeans(modelo)
plot(fit.means, errorbar=paste("ci",round((1-alfa)*100,0),sep=""))
par(o.par)

cat(bartitle("Post hoc tests"))

print(EMM <- emmeans::emmeans(modelo, "TamanhoGrupo"))
print(plot(EMM,  colors = "black", 
     xlab="Ideias",
     ylab=e))
mc <- multcomp::glht(modelo, linfct = multcomp::mcp(TamanhoGrupo = "Tukey"))
print(mcs <- summary(mc, test=multcomp::adjusted("bonferroni")))
print(multcomp::cld(mcs, level=alfa, decreasing=TRUE))
plot(mc,las=3)
mc <- multcomp::glht(modelo, linfct = multcomp::mcp(TamanhoGrupo = "Dunnett"))
mcs <- summary(mc, test=multcomp::adjusted("bonferroni"))
print(mcs)
plot(mc,las=3)

print(EMM <- emmeans::emmeans(modelo, "Profissional"))
print(plot(EMM,  colors = "black", 
     xlab="Ideias",
     ylab=e))
mc <- multcomp::glht(modelo, linfct = multcomp::mcp(Profissional = "Tukey"))
print(mcs <- summary(mc, test=multcomp::adjusted("bonferroni")))
print(multcomp::cld(mcs, level=alfa, decreasing=TRUE))
plot(mc,las=3)
mc <- multcomp::glht(modelo, linfct = multcomp::mcp(Profissional = "Dunnett"))
mcs <- summary(mc, test=multcomp::adjusted("bonferroni"))
print(mcs)
plot(mc,las=3)

# enable warnings
options(warn=0)
