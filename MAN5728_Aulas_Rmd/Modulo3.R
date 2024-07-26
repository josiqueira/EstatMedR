options(warn=-1)   
suppressMessages(library(knitr, warn.conflicts=FALSE))
suppressMessages(library(readxl, warn.conflicts=FALSE))
suppressMessages(library(DescTools, warn.conflicts=FALSE))
suppressMessages(library(effectsize, warn.conflicts=FALSE))
suppressMessages(library(MBESS, warn.conflicts=FALSE))
suppressMessages(library(RVAideMemoire, warn.conflicts=FALSE))
suppressMessages(library(corrplot, warn.conflicts=FALSE))
suppressMessages(library(gplots, warn.conflicts=FALSE))
suppressMessages(library(RcmdrMisc, warn.conflicts=FALSE))
suppressMessages(library(rcompanion, warn.conflicts=FALSE))
suppressMessages(library(Rmisc, warn.conflicts=FALSE))
suppressMessages(library(reshape2, warn.conflicts=FALSE))
suppressMessages(library(psych, warn.conflicts=FALSE))
suppressMessages(library(lsr, warn.conflicts=FALSE))
suppressMessages(library(ggplot2, warn.conflicts=FALSE))
suppressMessages(library(car, warn.conflicts=FALSE))
suppressMessages(library(rstatix, warn.conflicts=FALSE))
suppressMessages(library(jmv, warn.conflicts=FALSE))
suppressMessages(library(dplyr, warn.conflicts=FALSE))
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

Dados <- readxl::read_excel(path="Biometria_FMUSP.xlsx",
                            sheet="dados",
                            na=c("NA","na","Na","nA"))
Dados <- dplyr::mutate_if(Dados, is.character, as.factor)
categAF <- c("sempre_inativo",
            "atualmente_inativo",
            "baixa_intensidade",
            "media_intensidade",
            "alta_intensidade")
Dados$AtivFisica <- factor(Dados$AtivFisica,
                           levels=categAF,
                           labels=categAF)
categSexo <- c("M","F")
Dados$Sexo <- factor(Dados$Sexo,
                     levels=categSexo,
                     labels=categSexo)
Dados$MCT[Dados$MCT==658] <- NA
Dados$Estatura[Dados$Estatura==120] <- NA
# https://en.wikipedia.org/wiki/Body_mass_index
Dados$IMC.categ <- cut(Dados$IMC, 
                       breaks=c(0,18.5,25,Inf),
                       labels=c("sub","normal","sobre"),
                       ordered_result=TRUE)
Dados.F <- subset(Dados, Sexo=="F")
Dados.M <- subset(Dados, Sexo=="M")

alfa <- 0.05
estatura <- c(169, 174, 175, 186)
shapiro.test(estatura)
DescTools::ZTest(x=estatura, 
                 sd_pop=7,
                 mu=177,
                 alternative="two.sided",
                 conf.level=1-alfa,
                 na.rm=TRUE)
effectsize::interpret_cohens_d(d=0.14, rules="cohen1988")

alfa <- 0.05
q1 <- round(qnorm(p=alfa/2, mean=0, sd=1),2)
q2 <- round(qnorm(p=1-alfa/2, mean=0, sd=1),2)
DescTools::PlotProbDist(breaks=c(-3,q1,-0.29,0.29,q2,3), 
                        function(x) dnorm(x, mean=0, sd=1), 
                        blab=c(q1,-0.29,0.29,q2), 
                        alab=c("RejH0","p/2","","p/2","RejH0"),
                        xlim=c(-3,3),
                        main="normal(0,1)",                                              col=c("darkred","black",                                                "black","black","darkred"), 
                        density=c(20,20,0,20,20))

alfa <- 0.05
set.seed(123)
s <- rnorm(999, 175.9, 7)
estatura <- c(s, 176*1000-999*mean(s))
shapiro.test(estatura)
DescTools::ZTest(estatura, 
                 sd_pop=7,
                 mu=177,
                 alternative="two.sided",
                 conf.level=1-alfa,
                 na.rm=TRUE)
effectsize::interpret_cohens_d(d=0.14, rules="cohen1988")

alfa <- 0.05
q1 <- round(qnorm(p=alfa/2, mean=0, sd=1),2)
q2 <- round(qnorm(p=1-alfa/2, mean=0, sd=1),2)
DescTools::PlotProbDist(breaks=c(-5,-4.52,q1,q2,4.52,5), 
                        function(x) dnorm(x, mean=0, sd=1), 
                        blab=c(-4.52,q1,q2,4.52), 
                        alab=c("","RejH0","","RejH0",""),
                        xlim=c(-5,5),
                        main="normal(0,1)",                                              col=c("darkred","darkred",                                             "black","darkred","darkred"),                              density=c(20,20,0,20,20))

# Análise de poder retrospectivo (_plug in_) de ANOVA unifatorial independente balanceada (Gerard et al., 1998)

k <- 3 # numero de condicoes independentes
n <- 20 # numero de observacoes independentes de cada condicao
alfa <- 0.05
dfn <- k - 1
dfd <- k*(n - 1)
Fcrt <- qf(1-alfa, dfn, dfd)
Fobs <- Fcrt*0.99 
eta2 <- dfn*Fobs/(dfn*Fobs+dfd)
eta2lims <- MBESS::ci.pvaf(Fobs, dfn, dfd, k*n, 1-alfa)
f2 <- eta2/(1-eta2)
f2.ll <- eta2lims$Lower.Limit.Proportion.of.Variance.Accounted.for/
  (1-eta2lims$Lower.Limit.Proportion.of.Variance.Accounted.for)
f2.ul <- eta2lims$Upper.Limit.Proportion.of.Variance.Accounted.for/
  (1-eta2lims$Upper.Limit.Proportion.of.Variance.Accounted.for)
ncp.p <- dfd*f2 # ou dfn*Fobs
ncp.p.ll <- dfd*f2.ll 
ncp.p.ul <- dfd*f2.ul 
cat(paste("n =", k*n, "\tk =", k, "\tFcrt =", round(Fcrt,2),"\tFobs =", round(Fobs,2),"\n"))
poder.p <- 1-pf(Fcrt,dfn, dfd, ncp.p)
cat(paste("Estimativa pontual do poder retrospectivo =", round(poder.p,3),"\n"))
poder.p.ll <- 1-pf(Fcrt,dfn, dfd, ncp.p.ll)
cat(paste("Limite inferior do IC95% do poder retrospectivo =", round(poder.p.ll,3),"\n"))
poder.p.ul <- 1-pf(Fcrt,dfn, dfd, ncp.p.ul)
cat(paste("Limite superior do IC95% do poder retrospectivo =", round(poder.p.ul,3),"\n"))

tipsang.freq <- as.numeric(table(Dados$TipoSang))
tipsang.prob <- c(0.08, 0.34, 0.005, 0.025,
                  0.02, 0.08, 0.09, 0.36)
tslab <- c("A-", "A+", "AB-", "AB+", 
           "B-", "B+", "O-", "O+")
plot(tipsang.prob, type="p", lty=1, 
     lwd=1, pch=2, col="black", 
     xaxt = "n", xlab='Tipo Sanguineo', ylab="Probabilidade")
axis(1, at=1:length(tslab), labels=tslab)
lines(tipsang.freq/sum(tipsang.freq), pch=21, 
      type="p", lty=1, lwd=1, col="black")
legend("top", legend=c("Prob.Hipotet","Prob.Observ"), 
       pch=c(2,21), bty="n")
cat(tslab,"\n")
print(tipsang.freq)
print(round(tipsang.freq/sum(tipsang.freq), 3))
print(tipsang.prob)
out <- chisq.test(x=tipsang.freq, 
                  p=tipsang.prob,
                  simulate.p.value=TRUE, 
                  B=1e4) 
df <- chisq.test(tipsang.freq, p=tipsang.prob)$parameter
X2 <- out$statistic
p <- out$p.value
n <- sum(chisq.test(tipsang.freq, p=tipsang.prob)$observed)
cat("X^2 = ", X2 , " df = ",  df, " p = ", p, "\n", sep="")
cat("Heuristica de significancia: X^2/df > 2\n", round(X2/df,2), "\n")
cat("Residuos estandardizados\n", round(out$stdres,2), "\n")
V <- sqrt((X2/n)/df)
cat("V de Cramer = ", round(V,2), "\n")
cat("Grau de não aderência:\n")
print(effectsize::interpret_cramers_v(V))
RVAideMemoire::multinomial.multcomp(tipsang.freq, p.method="holm")

tabela <- xtabs(~TipoSang+Sexo, data=Dados)
tabela
gplots::balloonplot(t(tabela), main ="", xlab ="", ylab="",
                    label=TRUE, show.margins=TRUE,
                    dotcolor="gray")
res <- chisq.test(tabela,
                  simulate.p.value=TRUE, 
                  B=1e4)
res
df <- chisq.test(tabela)$parameter
cat("Graus de liberdade (df) = ", df, "\n")
x2 <- res$statistic
n <- sum(res$observed)
r <- nrow(tabela) 
c <- ncol(tabela) 
df <- (r-1)*(c-1) 
cat("Heuristica de significancia: X^2/df > 2\n", round(x2/df,2), "\n")
STAR <- res$stdres 
cat("MCSTAR\n") # Moment-Corrected STandardized Adjusted Residual
print(MCSTAR <- STAR/(sqrt((1-1/r)*(1-1/c)))) 
alfa <- 0.05
alfaBonf <- alfa/df
zcrit <- abs(qnorm(alfaBonf/2))
cat("|MCSTAR critico| (alfaBonferroni=5%/",df,") =",zcrit,"\n\n")
pSTAR <- (1-pnorm(abs(MCSTAR)))*2
pSTAR <- abs(MCSTAR)<abs(zcrit)
pSTAR <- pSTAR*1
if(prod(pSTAR)!=1)
{
  corrplot::corrplot(MCSTAR, is.corr = FALSE, 
                     insig="label_sig",
                     p.mat=pSTAR,
                     pch="*", 
                     pch.cex=3, 
                     pch.col="brown")  
} else
{
  pSTAR <- pSTAR*0
  corrplot::corrplot(MCSTAR, 
                     is.corr = FALSE, 
                     p.mat=pSTAR)  
}
V <- DescTools::CramerV(tabela)
cat("V de Cramer = ", round(V,2), "\n")
effectsize::interpret_cramers_v(V)

alfa <- 0.05
estatura <- c(169, 174, 175, 186)
shapiro.test(estatura)
fit <- t.test(x=estatura, 
       mu=177,
       alternative="two.sided",
       conf.level=1-alfa)
fit
d <- lsr::cohensD(estatura, mu=177)
cat("d de Cohen =", round(d,2), "\n")
effectsize::interpret_cohens_d(d=d, rules="cohen1988")
df <- fit$parameter
q1 <- round(qt(p=alfa/2, df=df),2)
q2 <- round(qt(p=1-alfa/2, df=df),2)
t <- round(abs(fit$statistic),2)
DescTools::PlotProbDist(breaks=c(-4.5,q1,-t,t,q2,4.5), 
                        function(x) dt(x, df=df), 
                        blab=c(q1,-t,t,q2), 
                        alab=c("RejH0","p/2","","p/2","RejH0"),
                        xlim=c(-4.5,4.5),
                        main=paste0("t(",df,")"),
                        col=c("darkred","black",
                              "black","black","darkred"), 
                        density=c(20,20,0,20,20))

alfa <- 0.05
psych::describeBy(Dados$MCT, group=Dados$Sexo, mat=2)
boxplot(MCT ~ Sexo, 
        data=Dados, 
        ylab="MCT (kg)", 
        xlab="Sexo")
car::densityPlot(MCT ~ Sexo, 
                 data=Dados, 
                 col=c("black","black"),
                 rug=FALSE)
gplots::plotmeans(MCT ~ Sexo, 
                  data=Dados, 
                  p=1-alfa/2,
                  xlab="Instructor", 
                  ylab="MCT (mg/semana)", 
                  main="IC95% Bonferroni da média de MCT",
                  barcol="black",
                  connect=FALSE)
RcmdrMisc::normalityTest(MCT ~ Sexo, data=Dados)
cat("IC95% das medias brutas estimadas de MCT\n")
Rmisc::group.CI(MCT ~ Sexo, data=Dados, ci=1-alfa/2)
fit <- t.test(MCT ~ Sexo, 
              data=Dados)
fit
d <- lsr::cohensD(MCT ~ Sexo, 
                  data=Dados,
                  method="unequal")
cat("d de Cohen =", round(d,2), "\n")
effectsize::interpret_cohens_d(d)
df <- fit$parameter
t <- fit$statistic # estatistica de teste t
F <- t^2 # estatistica de teste F de Fisher
eta2 <- F/(F+df)
cat("eta^2 = R^2 = ",round(eta2,2),"\n")
effectsize::interpret_eta_squared(eta2, rules="cohen1992")
cat("IC95% Bonferroni das medias marginais estimadas de MCT\n")
Sum <- rcompanion::groupwiseMean(MCT ~ Sexo, 
                                 data=Dados, 
                                 conf=1-alfa/2, 
                                 digits=3, 
                                 traditional=FALSE, 
                                 percentile=TRUE,
                                 na.rm=TRUE)
print(Sum)
ggplot2::ggplot(Sum, 
                ggplot2::aes(x = Sexo,y = Mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),width=0.05,size=0.5)+
  ggplot2::geom_point(shape=15,size=4) + 
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title=ggplot2::element_text(face="bold")) +
  ggplot2::ggtitle("IC95 Bonferroni de media marginal de MCT (kg)") 

Tabela <- ("
Participante Semana1 Semana2
a            1200    1100
b            1400    1200
c            1350    1250
d             950    1050
e            1400    1200
f            1150    1250
g            1300    1350
h            1325    1350
i            1425    1325
j            1500    1525
k            1250    1225
l            1150    1125
")
Dados <- read.table(textConnection(Tabela),header=TRUE)
Dados$Participante <- factor(Dados$Participante)
dif <- Dados$Semana2 - Dados$Semana1
alfa <- 0.05
boxplot(dif, ylab="Sodio Depois-Antes")
car::densityPlot(dif)
shapiro.test(dif)
Dados_long <- reshape2::melt(Dados,
                             id.vars="Participante",
                             measure.vars=c("Semana1","Semana2"),
                             variable.name="Semana")
names(Dados_long) <- c("Participante", "Semana", "Sodio")
Dados_long
psych::describeBy(Sodio~Semana, data=Dados_long, mat=2, digits=2)
psych::describe(dif)

ggplot2::ggplot(Dados_long, 
                ggplot2::aes(x=Semana, 
                             y=Sodio, 
                             colour=Participante, 
                             group=Participante)) +
  ggplot2::geom_line() + 
  ggplot2::geom_point(shape=21, 
                      fill="white") + 
  ggplot2::ylab("Sodio (mg/dia)") +
  ggplot2::ggtitle("Perfil do Participante") +
  ggplot2::theme_bw()

Dados_long_ICIP <- Rmisc::summarySEwithin(Dados_long, 
                               measurevar="Sodio", 
                               withinvars="Semana",
                               idvar="Participante", 
                               na.rm=TRUE, 
                               conf.interval=1-alfa/2)
Dados_long_ICIP

ggplot2::ggplot(Dados_long_ICIP, 
                ggplot2::aes(x=Semana, 
                             y=Sodio)) +
  ggplot2::geom_errorbar(width=.1, 
                         ggplot2::aes(ymin=Sodio-ci, 
                                      ymax=Sodio+ci)) +
  ggplot2::geom_point(shape=21, 
                      size=3, 
                      fill="white") +
  ggplot2::ylab("Sodio (mg/dia)") +
  ggplot2::ggtitle("IC95% Bonferroni intraparticipantes") +
  ggplot2::theme_bw()

cat("Analise de significancia estatistica: valor p\n")
print(res <- t.test(dif,mu=0))
cat("Analise de significancia pratica: tamanho de efeito\n")
d <- lsr::cohensD(x=Dados$Semana2, 
                  y=Dados$Semana1,
                  method="paired")
cat("d de Cohen =", round(d,2), "\n")
effectsize::interpret_cohens_d(d)
t <- res$statistic
F <- t^2
df <- res$parameter
eta2 <- F/(F+df)
cat("eta^2 = R^2 = ", eta2, "\n\n")
effectsize::interpret_eta_squared(eta2, rules="cohen1992")

# ANOVA de Welch: MCT~AtivFisica (Masculino)

psych::describeBy(Dados.M$MCT, 
                  Dados.M$AtivFisica, 
                  mat=2, 
                  digits=2)
boxplot(MCT~AtivFisica, data=Dados.M)
car::densityPlot(MCT~AtivFisica, data=Dados.M)
RcmdrMisc::normalityTest(MCT~AtivFisica, data=Dados.M)
plot.design(MCT~AtivFisica, data=Dados.M)
s <- Rmisc::summarySE(Dados.M, 
                      measurevar="MCT", 
                      groupvars=c("AtivFisica"),
                      conf.interval = 1-alfa/(length(unique(Dados.M$AtivFisica))),
                      na.rm=
                        TRUE)
pd <- ggplot2::position_dodge(0.5) 
ggplot2::ggplot(s, 
                ggplot2::aes(x=AtivFisica, 
                             y=MCT)) + 
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-ci, 
                                      ymax=MCT+ci), 
                         width=.3, 
                         position=pd) +
  ggplot2::geom_line(position=pd) +
  ggplot2::geom_point(position=pd, 
                      size=3,
                      shape=21) +
  ggplot2::xlab("Nivel de Atividade Fisica") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("IC95% Bonferroni de MCT Masculino") +
  ggplot2::ylim(62,80) +  
  ggplot2::theme_bw() +
  ggplot2::theme(legend.justification=c(1,0),
                 legend.position=c(1,0))

cat("Teste omnibus")
fit.M <- oneway.test(MCT~AtivFisica, 
                     data=Dados.M, 
                     na.action=na.omit)
print(fit.M)
cat("Teste post hoc")
ph.M <- rstatix::games_howell_test(MCT~AtivFisica, 
                                   data=Dados.M)
print(as.data.frame(ph.M))
cat("Analise de significancia pratica: tamanho de efeito\n")
F <- as.numeric(fit.M$statistic)
dfn <- as.numeric(fit.M$parameter[1])
dfd <- as.numeric(fit.M$parameter[2])
eta2 <- dfn*F/(dfn*F+dfd)
effectsize::interpret_eta_squared(eta2, rules="cohen1992")




