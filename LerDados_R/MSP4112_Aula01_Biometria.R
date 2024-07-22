library(readxl)
library(finalfit)
library(car)
library(GGally)
library(ggplot2)
library(psych) 
library(ggpubr)
library(dplyr)
library(haven)
library(Hmisc)
library(sjPlot)
library(sampling) 
library(gmodels)
library(gplots) 
library(dplyr)
library(labelled)
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

# http://www.hiercourse.com/docs/advanced_plotting.html
# http://ecor.ib.usp.br/doku.php?id=03_apostila:start
# https://docs.ufpr.br/~aanjos/CE231/web/apostila.html#Q1-1-2
# https://pt.wikipedia.org/wiki/Distribui%C3%A7%C3%A3o_do_tipo_sangu%C3%ADneo_por_pa%C3%ADs
# Descriptive Statistics and Graphics
## http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics
# Plotting means and error bars (ggplot2)
## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

Dados <- readxl::read_excel(path="Biometria_FMUSP.xlsx",
                            sheet="dados",
                            na=c("NA","na","nA","Na"))
print(str(Dados))
print(summary(Dados))
print(sapply(Dados, class))
Dados <- dplyr::mutate_if(Dados, is.character, as.factor)
Dados <- dplyr::mutate_at(Dados, dplyr::vars(ID), as.factor)
print(summary(Dados))
print(sapply(Dados, class))
print(labelled::look_for(Dados[,-1]))
# Preparacao dos dados
print(head(Dados))
print(tail(Dados))

print(class(Dados$AtivFisica))
print(levels(Dados$AtivFisica))
print(summary(subset(Dados, select=c(AtivFisica))))
Dados$AtivFisica <- factor(Dados$AtivFisica,
                           levels=c("sempre_inativo",
                                    "atualmente_inativo",
                                    "baixa_intensidade",
                                    "media_intensidade",
                                    "alta_intensidade"))
print(class(Dados$AtivFisica))
print(levels(Dados$AtivFisica))
print(summary(subset(Dados, select=AtivFisica)))

print(sjPlot::view_df(subset(Dados, select=c(-ID))))
print(summary(subset(Dados, select=-ID)))
Dados$MCT[Dados$MCT==658] <- NA
Dados$Estatura[Dados$Estatura==120] <- NA
Dados$IMC <- Dados$MCT/((Dados$Estatura/100)^2)
print(summary(subset(Dados, select=c(MCT, Estatura))))
summarytools::view(summarytools::dfSummary(Dados))
saveRDS(Dados, "Biometria_FMUSP.rds")

Dados <- readRDS("Biometria_FMUSP.rds")
N <- nrow(Dados)
n <- 5
set.seed(123)
indices <- sample(x=1:N, size=n, replace=FALSE)
amostra <- Dados[indices,]
print.data.frame(amostra)

set.seed(123)
index <- sampling::srswor(n=n, N=N) # Simple random sampling without replacement
amostra <- Dados[which(index!=0),]
print.data.frame(amostra)

set.seed(123)
amostra <- dplyr::sample_n(tbl=Dados, size=n, replace=FALSE)
print.data.frame(amostra)

Dados.F <- subset(Dados, Sexo=="F")
Dados.M <- subset(Dados, Sexo=="M")

# Missing Value Analysis
print(sapply(Dados,function(x){sum(!is.na(x))}))
print(sapply(Dados,function(x){sum(is.na(x))}))
n.total <- nrow(Dados)
n.completo <- nrow(na.omit(Dados))
n.incompleto <- n.total - n.completo
cat("Numero de casos total = ", n.total, "\n", sep="")
cat("Numero de casos completos = ", n.completo, 
    " (",round(100*n.completo/n.total,2),"%)\n", sep="")
cat("Numero de casos incompletos = ", n.incompleto, 
    " (",round(100*n.incompleto/n.total,2),"%)\n", sep="")
obs.falt <- sum(is.na(Dados))
obs.valid <- sum(!is.na(Dados))
obs.tot <- obs.falt + obs.valid
cat("Numero de observacoes validas = ", obs.valid, 
    " (",round(100*obs.valid/obs.tot,2),"%)\n", sep="")
cat("Numero de observacoes faltantes = ", obs.falt, 
    " (",round(100*obs.falt/obs.tot,2),"%)\n", sep="")
print(finalfit::missing_pattern(subset(Dados,select=-ID)))

# Analise descritiva numerica
# mean,median,25th and 75th quartiles,min,max
tapply(Dados$MCT,Dados$Sexo,summary)
# Tukey min,lower-hinge, median,upper-hinge,max
tapply(Dados$MCT,Dados$Sexo,fivenum)
# item name ,item number, nvalid, mean, sd,
# median, mad (Median Absolute Deviation), 
# min, max, skew, kurtosis, se
psych::describe(subset(Dados, select=c(MCT, Estatura)))
psych::describeBy(subset(Dados,select=c(MCT, Estatura)),
                  group=list(Dados$Sexo),
                  mat=TRUE,
                  digits=2)
psych::describeBy(subset(Dados,select=c(MCT, Estatura)),
                  group=list(Dados$Sexo, Dados$ABO),
                  mat=TRUE,
                  digits=2)
psych::describeBy(subset(Dados, select=c(MCT, Estatura)),
                  group=list(Dados$ABO, Dados$Sexo),
                  mat=TRUE,
                  digits=2)

agreg <- aggregate(subset(Dados, select=c(MCT, Estatura)),
                   by=list(Dados$Sexo),
                   FUN=mean,
                   na.rm=TRUE)
cat("\nMean\n")
print(agreg)
agreg <- aggregate(subset(Dados, select=c(MCT, Estatura)),
                   by=list(Dados$Sexo, Dados$ABO),
                   FUN=mean,
                   na.rm=TRUE)
cat("\nMean\n")
print(agreg)
agreg <- aggregate(subset(Dados, select=c(MCT, Estatura)),
                   by=list(Dados$ABO, Dados$Sexo),
                   FUN=mean,
                   na.rm=TRUE)
cat("\nMean\n")
print(agreg)

agreg <- aggregate(MCT ~ Sexo,
                   FUN=mean,
                   na.rm=TRUE,
                   data=Dados)
cat("\nMean\n")
print(agreg)

agreg <- aggregate(MCT ~ Sexo + ABO,
                   FUN=mean,
                   na.rm=TRUE,
                   data=Dados)
cat("\nMean\n")
print(agreg)

round(prop.table(table(Dados$TipoSang)),2)

# Analise descritiva grafica
# Frequencia de cada observacao
plot(Dados$Sexo, xlab="Sexo", ylab="Freq")
plot(Dados$TipoSang, xlab="Tipo Sanguineo", ylab="Freq")
plot(Dados$Sedentarismo, xlab="Sedentarismo", ylab="Freq")
plot(table(Dados$MCT), xlab="Massa corporal total (kg)", ylab="Freq")
plot(table(Dados$Estatura), xlab="Estatura (cm)", ylab="Freq")

# Grafico de setores
table(Dados$Sexo)
pie(table(Dados$Sexo), 
    xlab="Sexo")
table(Dados$ABO)
pie(table(Dados$ABO), 
    xlab="ABO")

# Tabela de contingencia
print(Sedentarismo.Sexo <- xtabs(~Sedentarismo+Sexo, data=Dados))
margin.table(Sedentarismo.Sexo,1)
margin.table(Sedentarismo.Sexo,2)
round(proportions(Sedentarismo.Sexo),4)
round(proportions(Sedentarismo.Sexo,1),4)
round(proportions(Sedentarismo.Sexo,2),4)
plot(Dados$Sedentarismo~Dados$Sexo, xlab="Sexo", ylab="Sedentarismo")
mosaicplot(~Sexo+Sedentarismo, data=Dados, color=FALSE)
gplots::balloonplot(t(Sedentarismo.Sexo), 
                    main ="Estudantes de Medicina\nFMUSP", 
                    xlab ="Sexo", 
                    ylab="Sedentarismo",
                    label=TRUE, 
                    show.margins=TRUE, 
                    show.zeros=TRUE, 
                    dotcolor="gray")
barplot(Sedentarismo.Sexo,
        beside=TRUE, 
        legend.text=rownames(Sedentarismo.Sexo),
        ylab="Freq",
        xlab="Sexo x Sedentarismo")
barplot(proportions(Sedentarismo.Sexo),
        beside=TRUE, 
        legend.text=rownames(Sedentarismo.Sexo),
        ylab="Freq",
        xlab="Sexo x Sedentarismo")

print(ABO.Sexo <- xtabs(~ABO+Sexo, data=Dados))
round(proportions(ABO.Sexo),4)
round(proportions(ABO.Sexo,1),4)
round(proportions(ABO.Sexo,2),4)
mosaicplot(~Sexo+ABO, data=Dados, color=FALSE)
gplots::balloonplot(t(ABO.Sexo), 
                    main ="Estudantes de Medicina\nFMUSP", 
                    xlab ="Sexo", 
                    ylab="ABO",
                    label=TRUE, show.margins=TRUE, 
                    show.zeros=TRUE, 
                    dotcolor="gray")
barplot(ABO.Sexo,
        beside=TRUE, 
        legend.text=rownames(ABO.Sexo),
        ylab="Freq",
        xlab="Sexo x ABO")
barplot(proportions(ABO.Sexo),
        beside=TRUE, 
        legend.text=rownames(ABO.Sexo),
        ylab="Freq",
        xlab="Sexo x ABO")

sexo.ABO.freq <- as.data.frame(table(Dados$Sexo, Dados$ABO))
names(sexo.ABO.freq) <- c("Sexo", "ABO", "Freq")
ggpubr::ggbarplot(sexo.ABO.freq, 
                  x="ABO", 
                  y="Freq",
                  color="Sexo",
                  palette=c("gray", "black"),
                  order=c("A", "B", "AB", "O"),
                  width=.7)

ggpubr::ggbarplot(sexo.ABO.freq, 
                  x="ABO", 
                  y="Freq",
                  color="Sexo",
                  palette=c("gray", "black"),
                  order=c("A", "B", "AB", "O"),
                  position = ggplot2::position_dodge(),
                  width=.7)

MCT.ci <- tapply(Dados$MCT, 
                 Dados$Sexo, 
                 gmodels::ci, 
                 na.rm=TRUE)
print(MCT.ci)

# Multiway tables: More than two categorical variables
print(xtabs(~Sexo + Sedentarismo + ABO, data=Dados))
mosaicplot(xtabs(~Sexo + Sedentarismo + ABO, data=Dados))
ftable(Sexo + Sedentarismo ~ ABO, data=Dados)

# boxplot
boxplot(Dados$MCT, horizontal=TRUE, 
        xlab="MCT (kg)")
rug(jitter(Dados$MCT))
boxplot(MCT~Sexo, data=Dados, horizontal=TRUE, 
        xlab="MCT (kg)")
boxplot(MCT~Sexo+Sedentarismo, data=Dados, horizontal=TRUE, 
        xlab="MCT (kg)")
boxplot(MCT~Sexo+Sedentarismo+Mao, data=Dados, horizontal=TRUE, 
        xlab="MCT (kg)", cex=0.6)

ggpubr::ggboxplot(data=Dados, 
                  y="MCT")
ggpubr::ggboxplot(data=Dados,
                  x="Sexo",
                  y="MCT", 
                  add="",
                  orientation="horizontal",
                  width=.7,
                  order=c("F", "M"))
ggpubr::ggboxplot(data=Dados,
                  x="Sexo",
                  y="MCT", 
                  add="",
                  orientation="horizontal",
                  width=.7,
                  order=c("M", "F"))
ggpubr::ggboxplot(data=Dados,
                  x="Sexo",
                  y="MCT", 
                  add="jitter",
                  orientation="horizontal",
                  width=.7,
                  order=c("F", "M"))
ggpubr::ggboxplot(data=Dados,
                  x="ABO",
                  y="MCT", 
                  add="",
                  orientation="horizontal",
                  width=.7,
                  select=c("A", "B", "O"),
                  order=c("O", "B", "A"))

# ECDF plot
ggpubr::ggecdf(data=Dados,
               x="MCT",
               linetype="Sexo")

# histograma
hist(Dados$MCT)
rug(jitter(Dados$MCT))

# bagplot
bgp.F <- DescTools::PlotBag(Dados.F$Estatura, 
                            Dados.F$MCT,
                            main=paste("Feminino"),
                            xlab="Estatura (cm)",
                            ylab="Massa Corporal Total (kg)",
                            na.rm=TRUE,
                            show.bagpoints=FALSE,
                            show.looppoints=FALSE,
                            show.whiskers=FALSE,
                            col.loophull="white",
                            col.looppoints="black", 
                            col.baghull="white",
                            col.bagpoints="black",
                            cex=1)
print(outliers.F <- as.data.frame(bgp.F$pxy.outlier))
for (o in 1:nrow(outliers.F))
{
  r.F <- which(Dados.F$Estatura==outliers.F$x[o] & 
               Dados.F$MCT==outliers.F$y[o])
  text(outliers.F$x[o],outliers.F$y[o], r.F, pos=1, cex=0.7)
}

bgp.M <- DescTools::PlotBag(Dados.M$Estatura, 
                          Dados.M$MCT,
                          main=paste("Masculino"),
                          xlab="Estatura (cm)",
                          ylab="Massa Corporal Total (kg)",
                          na.rm = TRUE,
                          show.bagpoints=FALSE,
                          show.looppoints=FALSE,
                          show.whiskers=FALSE,
                          col.loophull = "white",
                          col.looppoints = "black", 
                          col.baghull = "white",
                          col.bagpoints = "black",
                          cex=1)
print(outliers.M <- as.data.frame(bgp.M$pxy.outlier))
for (o in 1:nrow(outliers.M))
{
  r.M <- which(Dados.M$Estatura==outliers.M$x[o] & 
                 Dados.M$MCT==outliers.M$y[o])
  text(outliers.M$x[o], outliers.M$y[o], r.M, pos=1, cex=0.7)
}
# Comparacao de medias 
alpha <- 0.05
gplots::plotmeans(Dados$MCT~Dados$Sexo,
                  main="Intervalo de confiança de 95%\ncom correção de Bonferroni",
                  col="black",
                  barcol="black",
                  p=1-alpha/length(unique(levels(Dados$Sexo))),
                  connect=FALSE)
gplots::plotmeans(Dados$MCT~Dados$Sedentarismo,
                  main="Intervalo de confiança de 95%\ncom correção de Bonferroni",
                  col="black",
                  barcol="black",
                  p=1-alpha/length(unique(levels(Dados$Sexo))),
                  connect=FALSE)
gplots::plotmeans(Dados$MCT~ interaction(Dados$Sexo, 
                                         Dados$Sedentarismo),
                  main="Intervalo de confiança de 95%\ncom correção de Bonferroni",
                  col="black",
                  barcol="black",
                  p=1-alpha/(length(unique(levels(Dados$Sexo)))*
                    length(unique(levels(Dados$Sedentarismo)))),
                  connect=FALSE)

cor.test(Dados.F$MCT, as.numeric(Dados.F$Sedentarismo),
         method = "spearman")
cor.test(Dados.M$MCT, as.numeric(Dados.M$Sedentarismo),
         method = "spearman")

# Comparacao de distribuicoes
car::densityPlot(~MCT, data=Dados)
car::densityPlot(MCT~Sexo, data=Dados, 
                 col=c("black","black"))
car::densityPlot(MCT~Sedentarismo, data=Dados.F, 
                 col=c("black","black"),
                 main="Feminino")
car::densityPlot(MCT~Sedentarismo, data=Dados.M, 
                 col=c("black","black"),
                 main="Masculino")
car::densityPlot(MCT~ABO, data=Dados.F,
                 main="Feminino")
car::densityPlot(MCT~ABO, data=Dados.M,
                 main="Masculino")

# Grafico de dispersao
sunflowerplot(MCT~Estatura,
              main="Estatura x MCT: Feminino",
              data=Dados.F, 
              rotate=TRUE, 
              pch=1,
              size=.1,
              col="black", 
              seg.col="black",
              xlim=c(145,190),
              ylim=c(40,140),
              seg.lwd=.8)
sunflowerplot(MCT~Estatura,
              main="Estatura x MCT: Masculino",
              data=Dados.M, 
              rotate=TRUE, 
              pch=2,
              size=.1,
              col="black", 
              seg.col="black", 
              xlim=c(150,200),
              ylim=c(40,110),
              seg.lwd=.8,
              add=FALSE)

sunflowerplot(MCT~Estatura,
              main="Estatura x MCT: Feminino & Masculino",
              data=Dados.F, 
              rotate=TRUE, 
              pch=1,
              size=.1,
              col="black", 
              seg.col="black", 
              xlim=c(145,200),
              ylim=c(40,140),
              seg.lwd=.8)
sunflowerplot(MCT~Estatura,
              data=Dados.M, 
              rotate=TRUE, 
              pch=2,
              size=.1,
              col="black", 
              seg.col="black", 
              seg.lwd=.8,
              add=TRUE)

car::scatterplot(MCT~Estatura,
                 group=Dados$Sexo,
                 jitter=list(x=1, y=1),
                 regLine=FALSE, 
                 smooth=FALSE, 
                 ellipse=FALSE,
                 grid=FALSE,
                 col="black",
                 xlim=c(145,200),
                 ylim=c(30,130),
                 data=Dados)
car::scatterplot(MCT~Estatura,
                 group=Dados$Sexo, 
                 jitter=list(x=1, y=1),
                 regLine=FALSE, 
                 smooth=FALSE, 
                 boxplots=TRUE, 
                 ellipse=list(levels=c(0.68), 
                              robust=TRUE, 
                              fill=FALSE, 
                              fill.alpha=0.2),
                 grid=FALSE,
                 col="black", 
                 xlim=c(145,200),
                 ylim=c(30,130),
                 data=Dados)
car::scatterplot(MCT~Estatura,
                 group=Dados$Sexo, 
                 jitter=list(x=1, y=1),
                 regLine=FALSE, 
                 smooth=FALSE, 
                 ellipse=list(levels=c(0.68,0.999), 
                              robust=TRUE, 
                              fill=FALSE, 
                              fill.alpha=0.2),
                 grid=FALSE,
                 col="black",
                 xlim=c(145,200),
                 ylim=c(30,130),
                 data=Dados)
car::scatterplot(MCT~Estatura,
                 group=Dados$Sexo, 
                 jitter=list(x=1, y=1),
                 regLine=TRUE, 
                 smooth=FALSE, 
                 ellipse=FALSE,
                 grid=FALSE,
                 col="black",
                 xlim=c(145,200),
                 ylim=c(30,130),
                 data=Dados)

# Grafico matricial
car::scatterplotMatrix(Dados[,c("Estatura",
                                "MCT")], 
                       groups=Dados$Sexo,
                       jitter=list(x=1, y=1),
                       regLine=TRUE, 
                       smooth=FALSE, 
                       boxplots=TRUE, 
                       by.groups=TRUE,
                       ellipse=list(levels=c(0.5), 
                                    robust=TRUE, 
                                    fill=FALSE),
                       grid=FALSE,
                       col=c("#666666","#888888","#cccccc"), 
                       cex=0.5,
                       cex.labels=1,
                       row1attop=TRUE)
GGally::ggpairs(subset(Dados, 
                       select=-c(ID,Ano,Turma,Mao,TipoSang,
                                 ABO,AtivFisica,IMC)), 
                ggplot2::aes(colour=Sexo))

# Teste de normalidade
result <- MVN::mvn(data=subset(Dados, 
                               select=c(Sexo, MCT, Estatura)), 
                   subset="Sexo", 
                   mvnTest="hz", 
                   univariateTest="SW")
print(result$multivariateNormality)
print(result$univariateNormality)

