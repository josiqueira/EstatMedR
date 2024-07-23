source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")
alfa <- 0.05

cat(bartitle("Médias marginais estimadas ajustadas",2))

EMM <- emmeans::emmeans(ancova.fit, 
                        pairwise~"Grupo", 
                        adjust="holm",
                        level=1-alfa)
print(EMM$emmeans)
print(plot(EMM$emmeans,
           colors="black", 
           xlab="ErrosDirecao controlado por Experiencia",
           xlim=c(4,12),
           ylab="Grupo"))