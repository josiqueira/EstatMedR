# demo_ANOVA2f_descritiva.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(psych))
suppressMessages(library(lattice))
suppressMessages(library(RcmdrMisc))
suppressMessages(library(gplots))
suppressMessages(library(car))

source("eiras.bartitle.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

Dados <- readxl::read_excel("CafeinaAlcool_entre.xlsx")
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

o.par <- par()
m <- matrix(c(1, 1, 2, 2), nrow = 1, ncol = 4, byrow = TRUE)
layout(m)
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
par(o.par)

o.par <- par()
m <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4), nrow = 2, ncol = 4, byrow = TRUE)
layout(m)
with(Dados, 
     RcmdrMisc::plotMeans(NumErros, Alcool, Cafeina, 
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="Cafeina", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))
with(Dados, 
     RcmdrMisc::plotMeans(NumErros, Cafeina, Alcool,
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="Alcool", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))

car::densityPlot(NumErros~Cafeina, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f1))
car::densityPlot(NumErros~Alcool, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f2))
par(o.par)

m <- matrix(c(1, 1, 2, 2, 0, 3, 3, 0), nrow = 2, ncol = 4, byrow = TRUE)
layout(m)
plot.design(NumErros~Cafeina, data=Dados)
plot.design(NumErros~Alcool, data=Dados)
plot.design(NumErros~Cafeina + Alcool + Cafeina:Alcool, data=Dados)
par(o.par)

# enable warnings
options(warn=0)
