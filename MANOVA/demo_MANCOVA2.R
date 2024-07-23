suppressMessages(library(readxl, warn.conflicts = FALSE))
suppressMessages(library(jmv, warn.conflicts = FALSE))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")

options(warn=-1) # disable warnings

Dados <- readxl::read_excel("Adm2008.xlsx")
Dados <- Dados[, c("Nome","Genero","Estatura","MCT","Idade")]
Dados$Genero <- as.factor(Dados$Genero)

cat(bartitle("MANCOVA assuming equal slopes"))

fit <- lm(cbind(Estatura, MCT) ~ Genero + Idade, data=Dados)
print(car::Anova(fit, univariate=FALSE, multivariate=TRUE))
print(heplots::etasq(fit))

