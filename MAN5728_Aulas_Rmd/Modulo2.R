# "Módulo 2"
# José O Siqueira   siqueira@usp.br
# Paulo S P Silveira   silveira@usp.br
# "Tópicos Essenciais da Bioestatística"
# 2022 

# Descriptive Statistics and Graphics
## http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics
# Be Awesome in ggplot2: A Practical Guide to be Highly Effective - R software and data visualization
## http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization
# Plotting means and error bars (ggplot2)
## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# Carregar pacotes
options(warn=-1)
suppressMessages(library(knitr, warn.conflicts=FALSE))
suppressMessages(library(readxl, warn.conflicts=FALSE))
suppressMessages(library(psych, warn.conflicts=FALSE))
suppressMessages(library(HH, warn.conflicts=FALSE))
suppressMessages(library(DescTools, warn.conflicts=FALSE))
suppressMessages(library(MVN, warn.conflicts=FALSE))
suppressMessages(library(Rmisc, warn.conflicts=FALSE))
suppressMessages(library(ggplot2, warn.conflicts=FALSE))
suppressMessages(library(reshape2, warn.conflicts=FALSE))
suppressMessages(library(ggpubr, warn.conflicts=FALSE))
source("summarySEwithin2.R")

# Ler planilha 
Dados <- readxl::read_excel(path="Biometria_FMUSP.xlsx",
                            sheet="dados",
                            na="NA")
Dados$MCT[Dados$MCT==max(Dados$MCT,na.rm=TRUE)] <- NA
Dados$Estatura[Dados$Estatura==min(Dados$Estatura,na.rm=TRUE)] <- NA
Dados$ID <- factor(Dados$ID)
Dados$Ano <- factor(Dados$Ano)
Dados$Turma <- factor(Dados$Turma)
Dados$Sexo <- factor(Dados$Sexo)
Dados$Mao <- factor(Dados$Mao)
Dados$TipoSang <- factor(Dados$TipoSang)
Dados$ABO <- factor(Dados$ABO)
Dados$AtivFisica <- factor(Dados$AtivFisica)
Dados$Sedentarismo <- factor(Dados$Sedentarismo)
Dados.F <- subset(Dados, Sexo=="F")
Dados.M <- subset(Dados, Sexo=="M")
# Estimação de intervalo de confiança de MCT
# group |   n |  mean |    sd|  se| 
# :----:|----:|------:|-----:|---:|
# F     | 230 |  57.6 |   9.1| 0.6|     
# M     | 312 |  71.6 |  12.1| 0.7|

# IC95% por formula
alfa <- 0.05
## MCT Feminino
n.F <- sum(!is.na(Dados.F$MCT))
m.F <- mean(Dados.F$MCT, na.rm=TRUE)
sd.F <- sd(Dados.F$MCT, na.rm=TRUE)
se.F <- sd.F/sqrt(n.F)
cat("Erro-padrao da media de MCT Feminino = ", 
    round(se.F,1), "\n", sep="")
me.F <- qt(p=1-alfa/2, df=n.F-1)*se.F
cat("Margem de erro da media de MCT Feminino = ", 
    round(me.F,1), "\n", sep="")
ic.F <- m.F+c(-1,1)*me.F
cat("IC95(media de MCT Feminino) = [", 
    round(ic.F,1), "]\n")
## MCT Masculino
n.M <- sum(!is.na(Dados.M$MCT))
m.M <- mean(Dados.M$MCT, na.rm=TRUE)
sd.M <- sd(Dados.M$MCT, na.rm=TRUE)
se.M <- sd.M/sqrt(n.M)
cat("Erro-padrao da media de MCT Masculino = ", 
    round(se.M,1), "\n", sep="")
me.M <- qt(p=1-alfa/2, df=n.M-1)*se.M
cat("Margem de erro da media de MCT Masculino = ", 
    round(me.M,1), "\n", sep="")
ic.M <- m.M+c(-1,1)*me.M
cat("IC95(media de MCT Masculino) = [", 
    round(ic.M,1), "]\n")

# plot t-distribution
gl.F <- n.F-1
qt1.F <- round(qt(p=alfa/2, df=gl.F),2)
qt2.F <- round(qt(p=1-alfa/2, df=gl.F),2)
DescTools::PlotProbDist(breaks=c(-5, qt1.F, 0, qt2.F, 5), 
                        function(x) dt(x, df=gl.F), 
                        blab=c(qt1.F, "", qt2.F), 
                        xlim=c(-5,5), 
                        alab=c(alfa/2,"","", alfa/2),
                        col=c("black", "black"),
                        main=paste0("MCT Feminino: t(",gl.F,")"), 
                        density=c(20, 0, 0, 20))
gl.M <- n.M-1
qt1.M <- round(qt(p=alfa/2, df=gl.M),2)
qt2.M <- round(qt(p=1-alfa/2, df=gl.M),2)
DescTools::PlotProbDist(breaks=c(-5, qt1.M, 0, qt2.M, 5), 
                        function(x) dt(x, df=gl.M), 
                        blab=c(qt1.M, "", qt2.M), 
                        xlim=c(-5,5), 
                        alab=c(alfa/2,"","", alfa/2),
                        col=c("black", "black"),
                        main=paste0("MCT Masculino: t(",gl.M,")"), 
                        density=c(20, 0, 0, 20))

# `DescTools::MeanCI`: estimação de intervalo de confiança 
# IC95% por DescTools::MeanCI
round(DescTools::MeanCI(Dados.F$MCT, na.rm=TRUE),1)
round(DescTools::MeanCI(Dados.M$MCT, na.rm=TRUE),1)

# Os mesmos IC95% por t.test
round(t.test(Dados.F$MCT)$conf.int[1:2],1)
round(t.test(Dados.M$MCT)$conf.int[1:2],1)

# IC95% por DescTools::MeanCI com desvio-padrao conhecido
round(DescTools::MeanCI(Dados.F$MCT, sd=10, na.rm=TRUE), 1)
round(DescTools::MeanCI(Dados.M$MCT, sd=14, na.rm=TRUE), 1)

# IC95% unilateral por DescTools::MeanCI
# round(DescTools::MeanCI(Dados.F$MCT, sides="left", na.rm=TRUE),1)
# round(DescTools::MeanCI(Dados.M$MCT, sides="left", na.rm=TRUE),1)
# round(DescTools::MeanCI(Dados.F$MCT, sides="right", na.rm=TRUE),1)
# round(DescTools::MeanCI(Dados.M$MCT, sides="right", na.rm=TRUE),1)

# IC95% estratificado por Sexo
tapply(Dados$MCT, 
       Dados$Sexo, 
       FUN=DescTools::MeanCI, 
       na.rm=TRUE)

# IC95% por bootstrapping para sexo feminino
round(DescTools::MeanCI(Dados.F$MCT, 
                        method="boot", 
                        type="perc", 
                        R=1e4, 
                        na.rm=TRUE), 1)

# IC95% de MCT, Estatura e IMC para sexo feminino
round(do.call("rbind", lapply(Dados.F[, 10:12], 
                              FUN=DescTools::MeanCI, 
                              na.rm=TRUE)), 1)
t(round(sapply(Dados.F[,c("MCT","Estatura","IMC")], 
               FUN=MeanCI, 
               na.rm=TRUE), 1))
# Cookbook for R: Plotting means and error bars (ggplot2)
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
s <- Rmisc::summarySE(Dados, 
                      measurevar="MCT", 
                      groupvars=c("Sexo","Sedentarismo"),
                      na.rm=TRUE)

# Intervalo de confianca de 95%: Standard error of the mean (se)

pd <- ggplot2::position_dodge(0.5) # move them .05 to the left and right
ggplot2::ggplot(s, 
                ggplot2::aes(x=Sexo, 
                             y=MCT, 
                             colour=Sedentarismo)) + 
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-ci, 
                             ymax=MCT+ci), 
                width=.3, 
                position=pd) +
  ggplot2::geom_line(position=pd) +
  ggplot2::geom_point(position=pd, 
             size=3,
             shape=21) +
  ggplot2::xlab("Sexo") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("Intervalo de confiança de 95% de MCT") +
  ggplot2::ylim(50,80) +  
  ggplot2::theme_bw() +
  ggplot2::theme(legend.justification=c(1,0),
        legend.position=c(1,0))

# 95% Error bar: se
ggplot2::ggplot(s, 
                ggplot2::aes(x=Sexo, 
                             y=MCT, 
                             # group=Sedentarismo,
                             colour=Sedentarismo)) + 
  ggplot2::geom_bar(position=ggplot2::position_dodge(1), 
           stat="identity",
           width=.8,
           fill="white") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-ci, 
                             ymax=MCT+ci), 
                width=.3, 
                position=pd) +
  ggplot2::geom_line(position=pd) +
  ggplot2::geom_point(position=pd, 
             size=3,
             shape=21) +
  ggplot2::xlab("Sexo") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("Intervalo de confiança de 95% de MCT") +
  ggplot2::scale_y_continuous(breaks=0:100*10) +
  ggplot2::coord_cartesian(ylim=c(50,80)) +
  ggplot2::theme_bw()

# Intervalo de predicao >= 75%: Standard deviation (sd)
k75 <- 2
ggplot2::ggplot(s, 
                ggplot2::aes(x=Sexo, 
                             y=MCT, 
                             colour=Sedentarismo)) + 
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-k75*sd, 
                             ymax=MCT+k75*sd), 
                width=.3, 
                position=pd) +
  ggplot2::geom_line(position=pd) +
  ggplot2::geom_point(position=pd, 
             size=3,
             shape=21) +
  ggplot2::xlab("Sexo") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("Intervalo de predição de 95% de MCT") +
  ggplot2::ylim(30,100) + 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.justification=c(1,0),
        legend.position=c(1,0))

# 95% Error bar: sd
k75 <- 2
ggplot2::ggplot(s, 
                ggplot2::aes(x=Sexo, 
                             y=MCT, 
                             # group=Sedentarismo,
                             colour=Sedentarismo)) + 
  ggplot2::geom_bar(position=ggplot2::position_dodge(1), 
           stat="identity",
           width=.8,
           fill="white") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=MCT-k75*sd, 
                             ymax=MCT+k75*sd), 
                width=.3, 
                position=pd) +
  ggplot2::geom_line(position=pd) +
  ggplot2::geom_point(position=pd, 
             size=3,
             shape=21) +
  ggplot2::xlab("Sexo") +
  ggplot2::ylab("MCT (kg)") +
  ggplot2::ggtitle("Intervalo de predição >= 75% de MCT") +
  ggplot2::scale_y_continuous(breaks=0:100*10) +
  ggplot2::coord_cartesian(ylim=c(30,100)) +
  ggplot2::theme_bw()

# Within-subject CI
dfw <- read.table(header=TRUE, 
                  text='
 subject pretest posttest
       1    59.4     64.5
       2    46.4     52.4
       3    46.0     49.7
       4    49.0     48.7
       5    32.5     37.4
       6    45.2     49.5
       7    60.3     59.9
       8    54.3     54.1
       9    45.4     49.6
      10    38.9     48.5
 ')

# Treat subject ID as a factor
dfw$subject <- factor(dfw$subject)

# Convert to long format
dfw_long <- reshape2::melt(dfw,
                           id.vars="subject",
                           measure.vars=c("pretest","posttest"),
                           variable.name="condition")

dfw_long
#>    subject condition value
#> 1        1   pretest  59.4
#> 2        2   pretest  46.4
#> 3        3   pretest  46.0
#> 4        4   pretest  49.0
#> 5        5   pretest  32.5
#> 6        6   pretest  45.2
#> 7        7   pretest  60.3
#> 8        8   pretest  54.3
#> 9        9   pretest  45.4
#> 10      10   pretest  38.9
#> 11       1  posttest  64.5
#> 12       2  posttest  52.4
#> 13       3  posttest  49.7
#> 14       4  posttest  48.7
#> 15       5  posttest  37.4
#> 16       6  posttest  49.5
#> 17       7  posttest  59.9
#> 18       8  posttest  54.1
#> 19       9  posttest  49.6
#> 20      10  posttest  48.5
#> 

dfwc <- summarySEwithin2(dfw_long, 
                               measurevar="value", 
                               withinvars="condition",
                               idvar="subject", 
                               na.rm=TRUE, 
                               conf.interval=.95)
dfwc
#>   condition  N value value_norm       sd        se       ci
#> 1  posttest 10 51.43      51.43 2.262361 0.7154214 1.618396
#> 2   pretest 10 47.74      47.74 2.262361 0.7154214 1.618396
#> 

# Make the graph with the 95% confidence interval
ggplot2::ggplot(dfwc, 
                ggplot2::aes(x=condition, 
                             y=value)) +
  ggplot2::geom_line() +
  ggplot2::geom_errorbar(width=.1, 
                       ggplot2::aes(ymin=value-ci, 
                                    ymax=value+ci)) +
  ggplot2::geom_point(shape=21, 
                    size=3, 
                    fill="white") +
  ggplot2::ylim(40,60) +
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Within-subject CI95") +
  ggplot2::theme_bw()

# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
dfwc_between <- Rmisc::summarySE(data=dfw_long, 
                                 measurevar="value", 
                                 groupvars="condition", 
                                 na.rm=FALSE, 
                                 conf.interval=.95)
dfwc_between
#>   condition  N value       sd       se       ci
#> 1   pretest 10 47.74 8.598992 2.719240 6.151348
#> 2  posttest 10 51.43 7.253972 2.293907 5.189179

# Show the between-S CI's in red, and the within-S CI's in black
ggplot2::ggplot(dfwc_between, 
                ggplot2::aes(x=condition, 
                             y=value)) +
  ggplot2::geom_line() +
  ggplot2::geom_errorbar(width=.1, 
                ggplot2::aes(ymin=value-ci, 
                             ymax=value+ci), 
                colour="gray",
                data=dfwc_between) +
  ggplot2::geom_errorbar(width=.1, 
                ggplot2::aes(ymin=value-ci, 
                             ymax=value+ci), 
                colour="black",
                data=dfwc) +
  ggplot2::geom_point(shape=21, 
             size=3, 
             fill="white") +
  ggplot2::ylim(40, 60) +
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Within vs. Between-subject CI95") +
  ggplot2::theme_bw()

# Use a consistent y range
ymax <- max(dfw_long$value)
ymin <- min(dfw_long$value)

# Plot the individuals
ggplot2::ggplot(dfw_long, 
                ggplot2::aes(x=condition, 
                             y=value, 
                             colour=subject, 
                             group=subject)) +
  ggplot2::geom_line() + 
  ggplot2::geom_point(shape=21, 
             fill="white") + 
  ggplot2::ylim(ymin,ymax) +
  ggplot2::ylab("Value") +
  ggplot2::ggtitle("Subject profile") +
  ggplot2::theme_classic()

# Two within-subjects variables
data <- read.table(header=TRUE, 
                   text='
 Subject RoundMono SquareMono RoundColor SquareColor
       1        41         40         41          37
       2        57         56         56          53
       3        52         53         53          50
       4        49         47         47          47
       5        47         48         48          47
       6        37         34         35          36
       7        47         50         47          46
       8        41         40         38          40
       9        48         47         49          45
      10        37         35         36          35
      11        32         31         31          33
      12        47         42         42          42
')

# Convert it to long format
data_long <- reshape2::melt(data=data, 
                            id.var="Subject",
                            measure.vars=c("RoundMono", 
                                           "SquareMono", 
                                           "RoundColor", 
                                           "SquareColor"),
                            variable.name="Condition")
names(data_long)[names(data_long)=="value"] <- "Time"

# Split Condition column into Shape and ColorScheme
data_long$Shape <- NA
data_long$Shape[grepl("^Round",  data_long$Condition)] <- "Round"
data_long$Shape[grepl("^Square", data_long$Condition)] <- "Square"
data_long$Shape <- factor(data_long$Shape)

data_long$ColorScheme <- NA
data_long$ColorScheme[grepl("Mono$",  data_long$Condition)] <- "Monochromatic"
data_long$ColorScheme[grepl("Color$", data_long$Condition)] <- "Colored"
data_long$ColorScheme <- factor(data_long$ColorScheme, levels=c("Monochromatic","Colored"))

# Remove the Condition column now
data_long$Condition <- NULL

# Look at first few rows 
head(data_long)
#>   Subject Time Shape   ColorScheme
#> 1       1   41 Round Monochromatic
#> 2       2   57 Round Monochromatic
#> 3       3   52 Round Monochromatic
#> 4       4   49 Round Monochromatic
#> 5       5   47 Round Monochromatic
#> 6       6   37 Round Monochromatic

datac <- summarySEwithin2(data_long, 
                                measurevar="Time", 
                                withinvars=c("Shape",
                                             "ColorScheme"), 
                                idvar="Subject",
                                na.rm=TRUE)
datac
#>    Shape   ColorScheme  N     Time Time_norm       sd        se        ci
#> 1  Round       Colored 12 43.58333  43.58333 1.212311 0.3499639 0.7702654
#> 2  Round Monochromatic 12 44.58333  44.58333 1.331438 0.3843531 0.8459554
#> 3 Square       Colored 12 42.58333  42.58333 1.461630 0.4219364 0.9286757
#> 4 Square Monochromatic 12 43.58333  43.58333 1.261312 0.3641095 0.8013997

# Within-subject Error Bar 95%
ggplot2::ggplot(datac, 
                ggplot2::aes(x=Shape, 
                             y=Time, 
                             fill=ColorScheme)) +
  ggplot2::geom_bar(position=position_dodge(.9), 
           colour="black", 
           stat="identity") +
  ggplot2::geom_errorbar(position=position_dodge(.9), 
                width=.25, 
                ggplot2::aes(ymin=Time-ci, 
                             ymax=Time+ci)) +
  ggplot2::coord_cartesian(ylim=c(40,46)) +
  ggplot2::scale_fill_manual(values=c("#CCCCCC",
                             "#FFFFFF")) +
  ggplot2::scale_y_continuous(breaks=seq(1:100)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Within-subject CI95")

# Within-subject CI95
ggplot2::ggplot(datac, 
                ggplot2::aes(x=Shape, 
                             y=Time,
                             colour=ColorScheme)) +
  ggplot2::geom_errorbar(position=position_dodge(.9), 
                width=.25, 
                ggplot2::aes(ymin=Time-ci, 
                             ymax=Time+ci)) +
  ggplot2::geom_line(position=position_dodge(.9)) +
  ggplot2::geom_point(shape=21, 
             size=3, 
             fill="white",
             position=position_dodge(.9)) +
  ggplot2::coord_cartesian(ylim=c(41,46)) +
  ggplot2::scale_fill_manual(values=c("#CCCCCC",
                             "#FFFFFF")) +
  ggplot2::scale_y_continuous(breaks=seq(1:100)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Within-subject CI95")

# `HH::CIplot`: simulação de intervalo de confiança
HH::CIplot(n.intervals=1e2,
           n.per.row=40,
           pop.mean=175,
           pop.sd=10,
           conf.level=0.95)
HH::CIplot(n.intervals=1e4,
           n.per.row=40,
           pop.mean=175,
           pop.sd=10,
           conf.level=0.95)

set.seed(9342)
mp <- 175 # media populacional
dpp <- 8 # desvio-padrao populacional
cat ("Média populacional = ",mp,"\n",sep="")
cat ("Desvio padrão populacional = ",dpp,"\n",sep="")

n <- 30 # tamanho da amostra
cat("n =", n, "\n")
alfa <- 0.05 # nivel de significancia

plot(x = c(mp-dpp, mp+dpp), y = c(1, 100), type = "n", 
     xlab = paste("Estatura masculina (média = ",mp," cm)",sep=""), 
     ylab = "",
     axes=FALSE)
abline(v = mp, col = "red") # linha vertical da media populacional

# sampling
counter <- 0 # set counter to 0
B <- 1e5
for (i in 1:B)
{
  x <- rnorm(n, mp, dpp) # amostra aleatoria normal de tamanho n
  s <- sd(x) 
  L <- mean(x) - qt(1-alfa/2,n-1)*s/sqrt(n) # lower limit
  U <- mean(x) + qt(1-alfa/2,n-1)*s/sqrt(n) # upper limit
  if (L < mp && mp < U) # verifica se a media pop esta contida em IC95
  {
    color <- "#222222" # cinza
    counter <- counter + 1 # Se sim, soma 1 no contador
    espessura <- 0.5
  } else
  {
    color <- "#1965B0" # azul cobalto
    espessura <- 2
  }
  if (i <= 100) # plota os primeiros IC95%
    segments(L, i, U, i, col=color, lwd=espessura)
}
pIC95 <- counter/B # % de IC95 que contem a media populacional.
cat("\nProporcao dos ",
    format(B, scientific = FALSE),
    " IC95(media pop) que contem a media poppulacional ",
    mp, " = ",
    round(pIC95,2), sep="")


# `DescTools::MedianCI`: IC de mediana
DescTools::MedianCI(Dados.F$MCT, na.rm=TRUE)
DescTools::MedianCI(Dados.M$MCT, na.rm=TRUE)

# `DescTools::VarCI`: IC de desvio-padrão
sqrt(DescTools::VarCI(Dados.F$MCT, na.rm=TRUE))
sqrt(DescTools::VarCI(Dados.M$MCT, na.rm=TRUE))

# Variável com distribuição normal
# Exemplo de distribuição de normal: 1
## Pressão arterial sistólica de jovens saudáveis 

source("exemplo05_normal.R")

# Distribuição normal: Questão 1
# Qual é a probabilidade que nenhum paciente tenha atendimento completo durante uma madrugada no pronto-atendimento dessa pequena cidade?
# R.: $P(X > 140) = 0.023$ 
pnorm(q=140, mean=120, sd=10, lower.tail=FALSE)
# OU
1-pnorm(q=140, mean=120, sd=10)

# Distribuição normal: Questão 2
# Qual é a probabilidade dos valores de PAS de jovens sadios estarem entre 100 e 140 mmHg?
# R.: $P(100 < X < 140) = 0.9545$ 
pnorm(q=140, mean=120, sd=10)-pnorm(q=100, mean=120, sd=10)

# Distribuição normal: Questão 3
# Quais são os limites de um intervalo simétrico em relação à média que engloba 95% dos valores de PAS de jovens sadios?
# R.: $P(100.4 < X < 139.6) = 0.95$ 
qnorm(p=0.025, mean=120, sd=10)
qnorm(p=0.975, mean=120, sd=10)

DescTools::MeanCI(Dados.F$MCT, na.rm=TRUE)
DescTools::MeanCI(Dados.M$MCT, na.rm=TRUE)

describeBy(Dados$MCT,group=Dados$Sexo,mat=2)

m.F <- mean(Dados.F$MCT, na.rm=TRUE)
s.F <- sd(Dados.F$MCT, na.rm=TRUE)
plot(density(Dados.F$MCT, na.rm=TRUE),
     ylim=c(0, 0.06),
     main="Feminino",
     xlab="MCT (kg)",
     ylab="Densidade")
rug(jitter(Dados.F$MCT))
x.F <- seq(m.F-4*s.F, m.F+4*s.F, length.out=1e3)
y.F <- dnorm(x.F, m.F, s.F)
lines(x.F, y.F,lty=2)
legend("topright", legend=c("estimada", "normal"),
       lty=c(1,2), bty="n")

m.M <- mean(Dados.M$MCT, na.rm=TRUE)
s.M <- sd(Dados.M$MCT, na.rm=TRUE)
plot(density(Dados.M$MCT, na.rm=TRUE),
     ylim=c(0, 0.04),
     main="Masculino",
     xlab="MCT (kg)",
     ylab="Densidade")
rug(jitter(Dados.M$MCT))
x.M <- seq(m.M-4*s.M, m.M+4*s.M, length.out=1e3)
y.M <- dnorm(x.M, m.M, s.M)
lines(x.M, y.M,lty=2)
legend("topright", legend=c("estimada", "normal"),
       lty=c(1,2), bty="n")

result <- MVN::mvn(data=subset(Dados, 
                               select=c(Sexo, MCT)), 
                   subset="Sexo", 
                   mvnTest="hz", 
                   univariateTest="SW")
result$univariateNormality

# Teorema Central do Limite

# IC95% por bootstrapping da média para sexo feminino
round(DescTools::MeanCI(Dados.F$MCT, 
                        method="boot", 
                        type="perc", 
                        R=1e5, 
                        na.rm=TRUE), 1) 
# IC95% por bootstrapping da média para sexo masculino
round(DescTools::MeanCI(Dados.M$MCT, 
                        method="boot", 
                        type="perc", 
                        R=1e5, 
                        na.rm=TRUE), 1) 
# IC95% por bootstrapping da mediana para sexo feminino
round(DescTools::MedianCI(Dados.F$MCT, 
                        method="boot", 
                        R=1e5, 
                        na.rm=TRUE), 1) 
# IC95% por bootstrapping da mediana para sexo masculino
round(DescTools::MedianCI(Dados.M$MCT, 
                        method="boot", 
                        R=1e5, 
                        na.rm=TRUE), 1) 
# IC95% por bootstrapping do desvio-padrao para sexo feminino
round(sqrt(DescTools::VarCI(Dados.F$MCT, 
                      method="perc", 
                      R=1e5, 
                      na.rm=TRUE)),1)
# IC95% por bootstrapping do desvio-padrao para sexo masculino
round(sqrt(DescTools::VarCI(Dados.M$MCT, 
                      method="perc", 
                      R=1e5, 
                      na.rm=TRUE)),1)



