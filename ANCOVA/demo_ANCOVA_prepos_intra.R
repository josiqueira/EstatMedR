# suppress warnings
options(warn=-1)

suppressMessages(library(readxl))
suppressMessages(library(gplots))
suppressMessages(library(emmeans))
suppressMessages(library(multcomp))
suppressMessages(library(ggplot2))
suppressMessages(library(estimatr))
suppressMessages(library(tidyr))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras.showdataframe.R")

alfa <- 0.05
Dados <- readxl::read_excel("teste.xlsx")
Dados$Grupo <- factor(Dados$Grupo,
                      levels=c("droga A","droga B"))
Dados$Momento <- factor(Dados$Momento,
                      levels=c("Pre","Pos"))
n.f <- length(unique(Dados$Grupo))
# converte long -> wide
# preserva no formato long
Dados.long <- Dados
# converte para wide (para uso adiante)
Dados <- tidyr::spread(Dados.long,Momento,Escore)

cat(bartitle("Data"))
showdataframe(Dados.long)

cat(bartitle("Descriptive Statistics"))
print(xtabs(~Escore+Grupo+Momento, data=Dados.long))
print(ftable(Dados.long[,c("Grupo","Momento","Escore")]))
cat("\nMeans Pre:\n")
print(aggregate(Escore[Dados.long$Momento=="Pre"]~Grupo[Dados.long$Momento=="Pre"], data=Dados.long, FUN=mean))
cat("\nMeans Pos:\n")
print(aggregate(Escore[Dados.long$Momento=="Pos"]~Grupo[Dados.long$Momento=="Pos"], data=Dados.long, FUN=mean))

boxplot(Escore~Momento*Grupo, data=Dados.long, ylab="Escore")

car::densityPlot(Escore~Grupo, data=Dados.long, rug=TRUE, from=0, normalize=TRUE,
                 main="Escore (Pre)", subset=Momento=="Pre",
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f))
car::densityPlot(Escore~Grupo, data=Dados.long, rug=TRUE, from=0, normalize=TRUE,
                 main="Escore (Pos)", subset=Momento=="Pos",
                 na.rm=TRUE, ylab="Densidade", col=rep("black",n.f))

o.par <- par()
m <- matrix(c(1, 1, 2, 2, 0, 3, 3, 0), nrow = 2, ncol = 4, byrow = TRUE)
layout(m)
plot.design(Escore~Grupo, data=Dados.long)
plot.design(Escore~Momento, data=Dados.long)
plot.design(Escore~Grupo*Momento, data=Dados.long)
par(o.par)

cat(bartitle("Simple linear regressions"))

# formato wide eh melhor
car::scatterplot(Pos~Pre|Grupo, data=Dados, regLine=TRUE, smooth=FALSE, col="black")
# o mesmo com os dados no formato long
# car::scatterplot(Escore[Momento=="Pos"]~Escore[Momento=="Pre"]|Grupo[Momento=="Pre"], data=Dados.long,
#                  regLine=TRUE, smooth=FALSE, col="black")

# testes usam o formato wide
print(ggplot2::ggplot(Dados, 
                      aes(y=Pos, 
                          x=Pre, 
                          group=Grupo, 
                          linetype=Grupo,
                          shape=Grupo))+ 
        geom_point(alpha=1/4,
                   size=1)+ 
        theme_classic()+
        labs(title="RLS por Grupo",
             subtitle=paste0("BC",(1-alfa)*100,"%"),
             x="Escore Pre", 
             y="Escore Pos")+
        geom_smooth(method="lm",
                    na.rm=TRUE,
                    color="black",
                    level=1-alfa))

for (g in unique(Dados$Grupo))
{
  cat(bartitle(g,3))
  dt_tmp <- subset(Dados, Grupo==g)
  modelo <- lm(Pos~Pre, data=dt_tmp)
  print(summary(modelo))
}

cat(bartitle("ANCOVA"))

cat(bartitle("Assumptions",2))

## ANCOVA: Teste de dissociacao entre fator e covariavel
cat(bartitle("Dissociation between Grupo (factor) and VD Pre (covariate)",3))
modelo <- lm(Pre ~ Grupo, data=Dados)
print(Anova <- car::Anova(modelo,  white.adjust=TRUE))

## ANCOVA: Teste de igualdade das inclinacoes das retas de regressao
cat(bartitle("Homogenous slope of regressions",3))
modelo <- lm(Pos ~ Grupo + Pre + Grupo:Pre, 
             data=Dados)
print(Anova <- car::Anova(modelo,white.adjust=TRUE))

## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat(bartitle("Homogenous intercepts of regressions\n(assuming homogeneous slopes)\n- Factor Effect Test -",3))
ancova.fit <- lm(Pos~Grupo+Pre, 
                 data=Dados)
print(Anova <- car::Anova(ancova.fit, white.adjust=TRUE))
print(summary(ancova.fit))

cat(bartitle("Effect size analysis"))
cat(bartitle("Omnibus",2))
eta2 <- summary(ancova.fit)$r.squared
Eta2classification(eta2)
cat(bartitle("Partials",2))
eta2p <- heplots::etasq(ancova.fit)
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

cat(bartitle("Adjusted estimated marginal means"))

print(EMM <- emmeans::emmeans(ancova.fit, 
                              pairwise~"Grupo", 
                              adjust="tukey",
                              level=1-alfa))
print(plot(EMM$emmeans,
           colors="black", 
           main="Adjusted Estimated Marginal Means",
           xlab="VD Pos controlada por VD Pre",
           xlim=c(4,12),
           ylab="Grupo"))

cat(bartitle("Post hoc tests: Grupo (factor) adjusted by VD Pre (covariate)"))

# nomes do fator encurtados
Dados$Grupo <- as.character(Dados$Grupo)
fatores <- unique(as.character(Dados$Grupo))
letra <- "A"
legenda <- c()
levels <- c()
cat ("\nLegend:\n")
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  levels <- c(levels,letra)
  Dados$GrupoShort[Dados$Grupo==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
Dados$Grupo <- factor(Dados$Grupo,
                      levels=c("droga A","droga B"))
Dados$GrupoShort <- factor(Dados$GrupoShort,
                           levels=levels)

ancova.fitShort <- lm(Pos~GrupoShort+Pre, 
                 data=Dados)

mc <- multcomp::glht(ancova.fitShort,
                           linfct=multcomp::mcp("GrupoShort"="Tukey"))
confint <- confint(mc,level=1-alfa)
print(confint)
plot(confint, 
     xlim=c(min(confint$confint[,2],confint$confint[,3],0),
            max(confint$confint[,2],confint$confint[,3],0)),
     main=paste0("Tukey"," contrasts: Grupo\n",round((1-alfa)*100),"% family-wise CI"),
     las=3)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")

print(mcs <- summary(mc, test=multcomp::adjusted("bonferroni", 
                                                 level=1-alfa)))
print(multcomp::cld(mcs,level=alfa))

# enable warnings
options(warn=0)

