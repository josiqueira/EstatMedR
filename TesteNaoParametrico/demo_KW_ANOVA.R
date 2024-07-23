source("eiras.shape.test.R")

# ANOVA1f_indep_Welch_sodio.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as palavras
# Grupo pela nova VI (fator)
# Sintoma2 pela nova VD (resposta)

source("eiras_plotIC.R")

# suppress warnings
options(warn=-1)

alfa <- 0.05
TH <- as.data.frame(readxl::read_excel("Enxaqueca.xlsx"))
TH$Grupo <- factor(TH$Grupo, levels=unique(TH$Grupo))

print(TH)
cat("\nTabulando Sintoma2 por Grupo:")
print(table(TH$Sintoma2,TH$Grupo))

print(with(TH, psych::describeBy(Sintoma2,Grupo,digits=2)))

# teste de assimetria e normalidade das diferencas
s <- list()
s[["terapia"]] <- TH$Sintoma2[TH$Grupo=="Terapeuta"]
s[["autoaj"]] <- TH$Sintoma2[TH$Grupo=="Autoajuda"]
s[["espera"]] <- TH$Sintoma2[TH$Grupo=="Lista de espera"]
shape <- shape.test(s, labels = c("Terapeuta",
                                  "Autoajuda",
                                  "Lista de espera"))
boxplot(Sintoma2~Grupo,data=TH,
        ylab=names(TH)[which(names(TH)=="Sintoma2")],
        xlab=names(TH)[which(names(TH)=="Grupo")]
)
with(TH, gplots::plotmeans(Sintoma2~Grupo,
                           error.bars="conf.int", 
                           level=1-alfa/length(unique(TH$Grupo)),
                           connect=FALSE,
                           ylab=names(TH)[which(names(TH)=="Sintoma2")],
                           xlab=names(TH)[which(names(TH)=="Grupo")],
                           main="IC95% Bonferroni",
                           barcol="black"))
car::densityPlot(Sintoma2~Grupo, data=TH, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=c("black", "black", "black"))
cat("\n")

plot(ecdf(TH$Sintoma2[TH$Grupo=="Terapeuta"]), 
     main="Empirical Cumulative Distribution Function",
     xlim=c(0.5,5.5),
     xlab="Sintoma2", 
     ylab="Probabilidade",
     lwd=2, verticals = TRUE)
lines(ecdf(TH$Sintoma2[TH$Grupo=="Autoajuda"]), 
      lwd=2, col="blue", verticals = TRUE)
lines(ecdf(TH$Sintoma2[TH$Grupo=="Lista de espera"]), 
      lwd=2, col="brown", verticals = TRUE)
legend("topleft",
       c("Terapeuta", "Autoajuda", "Espera"),
       lty=1,
       lwd=2,
       col=c("black","blue","brown"),
       box.lwd=0, bg="transparent")

# nomes do fator encurtados
TH$Grupo <- as.character(TH$Grupo)
fatores <- unique(as.character(TH$Grupo))
letra <- "A"
legenda <- c()
cat ("\nLegenda:\n")
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  TH$Grupo[TH$Grupo==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
TH$Grupo <- as.factor(TH$Grupo)


cat("\nANOVA unifatorial independente de Welch\n\n")
alfa <- 0.05
VD <- names(TH)[which(names(TH)=="Sintoma2")]
VI <- names(TH)[which(names(TH)=="Grupo")]
cat("VD =", VD,"\n")
cat("Fator =", VI,"\n")
cat("\nAnalise de significancia estatistica: teste omnibus\n")
print(res <- jmv::anovaOneW(formula = Sintoma2~Grupo, data=TH,
                            desc=TRUE, descPlot = FALSE, phMethod ='gamesHowell',
                            phMeanDif = TRUE, phTest=TRUE, phFlag=TRUE))

cat("\nAnalise de significancia pratica: tamanho de efeito\n")
F <- as.numeric(res$anova$asDF[2])
dfn <- as.numeric(res$anova$asDF[3])
dfd <- as.numeric(res$anova$asDF[4])
eta2 <- dfn*F/(dfn*F+dfd)
if (0 <= eta2 & eta2 < 0.1) {geta2 <- "minimo"}
if (0.1 <= eta2 & eta2 < 0.6) {geta2 <- "pequeno"}
if (0.6 <= eta2 & eta2 < 0.14) {geta2 <- "intermediario"}
if (0.14 <= eta2 & eta2 <= 1.0) {geta2 <- "grande"}
cat("- eta^2 =", eta2, "\nGrau", geta2,
    "de explicacao da variancia da VD", VD,"pela VI", VI,"\n")
f2 <- eta2/(1-eta2) # tamanho de efeito f de Cohen
ncp <- dfd*f2 # parametro de nao-centralidade
fc <- qf(1-alfa, dfn, dfd, 0)
p <- 1-pf(F,dfn,dfd,0)
if (p < 1e-4)
{
  p <- sprintf("%.2e",p)
} else
{
  p <- sprintf("%.4f",p)
}
f <- seq(0,2*ncp,0.01)
densf <- df(f, dfn, dfd, 0)
plot(f, densf, xlab="F", ylab="densidade", lwd=2, type="l")
densf <- df(f, dfn, dfd, ncp)
lines(f,densf, lwd=2, lty=2)
abline(v=fc, lty=3)
abline(v=F, lty=4)
legend("topright",
       c("H0", "Obs", 
         paste("Fc(",dfn,",",round(dfd,3),") = ",round(fc,3),sep=""), 
         paste("Fobs = ",round(F,3),"\n",
               "p = ",p,sep="") 
       ), 
       lwd=c(2,2,1,1), lty=c(1,2,3,4))


cat("\nOutra opcao de teste posthoc com rstatix::games_howell_test\n")
print(res2 <- as.data.frame(rstatix::games_howell_test(Sintoma2~Grupo, data=TH)))
res2$g21 <- paste0(res2$group2,"-",res2$group1)

df_plot <- data.frame(
  res2$g21,
  res2$estimate,
  res2$conf.low,
  res2$conf.high,
  res2$p.adj
)
eiras_plotIC(df_plot,
             main="95% family-wise confidence level",
             xlab="Difference",
             usecolor="n"
)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")

# enable warnings
options(warn=0)
