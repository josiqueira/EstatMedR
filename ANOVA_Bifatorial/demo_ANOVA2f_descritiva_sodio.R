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

Dados <- read_excel("Nutricao2fatores.xlsx")
Dados$Instructor <- factor(Dados$Instructor)
Dados$Student <- factor(Dados$Student)
Dados$Supplement <- factor(Dados$Supplement)
fatores <- unique(as.character(Dados$Instructor))
letra <- "A"
legenda <- c()
cat ("\nLegenda:\n")
Dados$InstructorShort <- NA
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  Dados$InstructorShort[Dados$Instructor==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
Dados$InstructorShort <- as.factor(Dados$InstructorShort)
n.f1 <- length(unique(Dados$Instructor))
n.f2 <- length(unique(Dados$Supplement))

cat(bartitle("Data"))
showdataframe(Dados,4,3)

cat(bartitle("Descriptive Statistics"))

cat("\n\tEstratification by Instructor:\n")
print(with(Dados, psych::describeBy(x=Sodium,group=list(Instructor),digits=2)))
cat("\n\tEstratification by Supplement:\n")
print(with(Dados, psych::describeBy(x=Sodium,group=list(Supplement),digits=2)))
cat("\n\tEstratification by Instructor and Supplement:\n")
print(with(Dados, psych::describeBy(x=Sodium,group=list(Instructor,Supplement),digits=2)))

boxplot(Sodium~InstructorShort * Supplement, data=Dados,
        ylab="Sodium",
        xlab="Instructor (uppercase), Supplement (lowercase)"
)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")

print(grf <- lattice::xyplot(Sodium~Instructor, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(Sodium~Supplement, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(Sodium~InstructorShort:Supplement, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))
print(grf <- lattice::xyplot(Sodium~Instructor|Supplement, data=Dados, type=c("p","a"),
                             jitter.x=TRUE, col="black"))

with(Dados, gplots::plotmeans(Sodium~Instructor,
                              error.bars="conf.int", level=.95,
                              connect=FALSE,
                              ylab=names(Dados)[which(names(Dados)=="Sodium")],
                              xlab=names(Dados)[which(names(Dados)=="Instructor")],
                              main="IC95%",
                              barcol="black"))

with(Dados, 
     RcmdrMisc::plotMeans(Sodium, Instructor, Supplement,
                          error.bars="conf.int", level=.95, connect=TRUE,
                          xlab="Instructor", ylab="Lenght", main="CI95%",
                          pch=5:(5+n.f2), col="black",
                          legend.pos="farright"))

car::densityPlot(Sodium~Instructor, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f1))
car::densityPlot(Sodium~Supplement, data=Dados, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f2))

plot.design(Sodium~Instructor, data=Dados)
plot.design(Sodium~Supplement, data=Dados)
plot.design(Sodium~Instructor + Supplement + Instructor:Supplement, data=Dados)
