# https://datasus.saude.gov.br/transferencia-de-arquivos/ 
# SIM - Sistema de Informacoes de Mortalidade
# DO - Declaracoes de Obitos 
# 2020 - BR 
# 1.556.824 linhas x 88 variáveis
library(dplyr)
library(sjPlot)
library(summarytools)
library(kableExtra) 
library(data.table)
library(psych)
library(read.dbc)
library(lubridate)
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

age <- function(dob, age.death, units="years", floor=TRUE) {
  calc.age <- lubridate::interval(dob, age.death)/
    lubridate::duration(num = 1, units=units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
df <- read.dbc::read.dbc("DOBR2020.dbc")
write.csv(df,"DOBR2020.csv")
Dados <- data.table::fread(input="DOBR2020.csv",
                           stringsAsFactors=TRUE,
                           showProgress=TRUE)
Dados <- subset(Dados,select=-V1)
Dados$DTOBITO <- as.Date(as.character(Dados$DTOBITO),
                         format="%d%m%Y")
Dados$DTNASC <- as.Date(as.character(Dados$DTNASC),
                        format="%d%m%Y")
Dados$IdadeObito <- age(Dados$DTNASC, Dados$DTOBITO)
Dados$IdadeObito[Dados$IdadeObito < 0 | Dados$IdadeObito > 120] <- NA
Dados <- subset(Dados, 
                select=-c(19, 47, 74, 81, 85))
sjPlot::view_df(Dados)
Dados$TIPOBITO <- factor(Dados$TIPOBITO,
                         levels=c(1,2),
                         labels=c("Fetal",
                                  "Não fetal"))
Dados$LOCOCOR <- factor(Dados$LOCOCOR,
                        levels=c(1:6,9),
                        labels=c("Hospital",
                                 "Outros estabelecimentos de saúde",
                                 "Domicílio",
                                 "Via pública",
                                 "Outros",
                                 "Aldeia indígena",
                                 "Ignorado"))
Dados$SEXO <- factor(Dados$SEXO,
                     levels=0:2,
                     labels=c("Ignorado",
                              "Masculino",
                              "Feminino"))
Dados$RACACOR <- factor(Dados$RACACOR,
                        levels=1:5,
                        labels=c("Branca",
                                 "Preta",
                                 "Amarela",
                                 "Parda",
                                 "Indígena"))
Dados$ESTCIV <- factor(Dados$ESTCIV,
                       levels=c(1:5,9),
                       labels=c("Solteiro",
                                "Casado",
                                "Viúvo",
                                "Separado judicialmente/ divorciado",
                                "União Estável",
                                "Ignorado"))
Dados$ESC2010 <- factor(Dados$ESC2010,
                        levels=c(0:5,9),
                        labels=c("Sem",
                                 "Fundamental I",
                                 "Fundamental II",
                                 "Médio",
                                 "Superior incompleto",
                                 "Superior completo",
                                 "Ignorado"))
Dados$CIRCOBITO <- factor(Dados$CIRCOBITO,
                         levels=c(1:4,9),
                         labels=c("Acidente",
                                  "Suicídio",
                                  "Homicídio",
                                  "Outros",
                                  "Ignorado"))
Dados$ACIDTRAB <- factor(Dados$ACIDTRAB,
                         levels=c(1,2,9),
                         labels=c("Sim",
                                  "Não",
                                  "Ignorado"))
summary(Dados[,c("TIPOBITO","IdadeObito", "SEXO",
                 "RACACOR", "ESTCIV", "ESC2010","PESO",
                 "LOCOCOR","CIRCOBITO", "ACIDTRAB")])
saveRDS(Dados, "DOBR2020.rds")
Dados <- readRDS("DOBR2020.rds")
Variaveis <- c("TIPOBITO", "IdadeObito", "SEXO",
               "RACACOR", "ESTCIV", "ESC2010", "PESO",
               "LOCOCOR", "CIRCOBITO", "ACIDTRAB")
Dados <- subset(Dados, select=Variaveis)
summary(Dados)
sjPlot::view_df(Dados)
summarytools::view(summarytools::dfSummary(Dados))

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


pie(table(Dados$CIRCOBITO), main="Tipo de óbito")
xtabs(~ Dados$CIRCOBITO)
round(prop.table(xtabs(~ Dados$CIRCOBITO)),2)
pie(table(Dados$LOCOCOR), main="Local de ocorrência do óbito")
xtabs(~ Dados$LOCOCOR)
round(prop.table(xtabs(~ Dados$LOCOCOR)),2)
plot(xtabs(~ Dados$LOCOCOR + Dados$CIRCOBITO))
tabela <- xtabs(~ Dados$LOCOCOR + Dados$CIRCOBITO)
print(tabela)
round(prop.table(tabela),2)
gplots::balloonplot(t(tabela), main ="Declarações de Óbito\nBrasil 2020", 
                    xlab ="Tipo de óbito", 
                    ylab="Local ocorrência",
                    label=TRUE, show.margins=TRUE, 
                    show.zeros=TRUE, 
                    dotcolor="gray")
summarytools::st_options(lang="pt")
section_title <- "**DOBR2020**"
summarytools::define_keywords(title.freq=section_title,
                              freq="n")
sjPlot::view_df((Dados))
summarytools::view(summarytools::dfSummary(Dados))
summarytools::view(summarytools::freq(Dados[,"SEXO"], 
                                      plain.ascii=FALSE, 
                                      style="rmarkdown"))
summarytools::view(summarytools::freq(Dados[,"SEXO"], 
                                      plain.ascii=FALSE, 
                                      style="rmarkdown",
                                      report.nas=FALSE))
summarytools::view(summarytools::freq(Dados[,"SEXO"], 
                                      plain.ascii=FALSE, 
                                      style="rmarkdown",
                                      report.nas=FALSE,
                                      totals=FALSE, 
                                      cumul=FALSE, 
                                      headings=FALSE))
summarytools::view(summarytools::freq(Dados,
                                      plain.ascii=FALSE,
                                      style="rmarkdown"))

summarytools::view(summarytools::freq(Dados,
                                      plain.ascii=FALSE,
                                      style="rmarkdown",
                                      order="freq",
                                      rows=1:5))
summarytools::view(summarytools::ctable(Dados$SEXO, 
                                        Dados$RACACOR,
                                        prop='n',
                                        totals=FALSE, 
                                        headings=TRUE,
                                        useNA="no"))
summarytools::view(summarytools::ctable(Dados$SEXO, 
                                        Dados$RACACOR,
                                        prop="t"))
summarytools::view(summarytools::descr(Dados,
                                       transpose=TRUE,
                                       headings=TRUE))
summarytools::view(summarytools::descr(Dados,
                                       stats=c("mean", "sd","n.valid"),
                                       transpose=TRUE,
                                       headings=TRUE))
summarytools::view(stby(Dados,
                        INDICES=list(Dados$SEXO),
                        FUN=summarytools::descr, 
                        stats="common", 
                        transpose=TRUE))
summarytools::view(stby(data=list(Dados$SEXO, Dados$RACACOR),
                        INDICES=list(Dados$ESTCIV), 
                        FUN=summarytools::ctable,
                        prop="n",
                        totals=FALSE, 
                        headings=TRUE,
                        useNA="no"))
summarytools::view(stby(data=list(Dados$SEXO, Dados$RACACOR),
                        INDICES=list(Dados$ESTCIV), 
                        FUN=summarytools::ctable,
                        prop="t"))
summarytools::view(Dados %>% 
                     dplyr::group_by(SEXO) %>% 
                     summarytools::descr(stats=c("mean", "sd","n.valid"),
                                         transpose=TRUE,
                                         headings=TRUE))
summarytools::stby(data=Dados, 
                   INDICES=Dados$SEXO, 
                   FUN=summarytools::descr, 
                   stats=c("mean", "sd","n.valid")) %>%
  summarytools::tb(order=3) %>%
  kableExtra::kable(format="html", digits=2) %>%
  kableExtra::collapse_rows(columns=1, valign="top")

# car::densityPlot(Dados$IdadeObito~Dados$SEXO) # demora demais!
psych::describeBy(Dados$IdadeObito, Dados$SEXO, 
                  mat=1, digits=2)
plot(table(Dados$IdadeObito))
boxplot(Dados$IdadeObito~Dados$SEXO, horizontal=TRUE)
table(Dados$IdadeObito)
ggplot2::ggplot(Dados, 
                ggplot2::aes(IdadeObito, 
                             fill=SEXO, 
                             colour=SEXO)) +
  ggplot2::geom_density(alpha=0.2) +
  ggplot2::theme_bw()

ggplot2::ggplot(Dados, 
                ggplot2::aes(IdadeObito, 
                             fill=RACACOR, 
                             colour=RACACOR)) +
  ggplot2::geom_density(alpha=0.2) +
  ggplot2::theme_bw()

ggplot2::ggplot(Dados, 
                ggplot2::aes(IdadeObito, 
                             fill=SEXO:RACACOR, 
                             colour=SEXO:RACACOR)) +
  ggplot2::geom_density(alpha=0.2) +
  ggplot2::theme_bw()

ggplot2::ggplot(Dados, 
                ggplot2::aes(PESO, 
                             fill=SEXO, 
                             colour=SEXO)) +
  ggplot2::geom_density(alpha=0.2) +
  ggplot2::theme_bw()

ggplot2::ggplot(Dados, 
                ggplot2::aes(PESO, 
                             fill=RACACOR, 
                             colour=RACACOR)) +
  ggplot2::geom_density(alpha=0.2) +
  ggplot2::theme_bw()

ggplot2::ggplot(Dados, 
                ggplot2::aes(PESO, 
                             fill=SEXO:RACACOR, 
                             colour=SEXO:RACACOR)) +
  ggplot2::geom_density(alpha=0.2) +
  ggplot2::theme_bw()




