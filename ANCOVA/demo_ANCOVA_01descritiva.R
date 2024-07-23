source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")

cat(bartitle("Dados"))
print.data.frame(Dados)
cat(bartitle("Análise descritiva"))
print(xtabs(~Grupo, data=Dados))
ftable(Dados$ID, Dados$Grupo)

cat("\n\nSumário:\n")
print(psych::describeBy(ErrosDirecao~Grupo,
                        mat=1,
                        digits=2,
                        data=Dados))
print(GGally::ggpairs(Dados[-1], ggplot2::aes(colour=Grupo)))
