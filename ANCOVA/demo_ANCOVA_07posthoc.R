source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")
alfa <- 0.05

cat(bartitle("Teste post hoc: Grupo (fator) ajustado por Experiencia (covariável)"))

print(summary(EMM$contrasts, infer=TRUE))
print(plot(EMM$contrasts, 
           colors="black"))
print(multcomp::cld(object=EMM$emmeans,
                    level=1-alfa,
                    adjust="holm",
                    Letters=letters,
                    alpha=alfa))

cat(bartitle("Teste post hoc: Grupo (fator) ajustado por Experiencia (covariável): Dunnett"))
EMM.contrast <- emmeans::contrast(EMM, 
                                  method="trt.vs.ctrl", 
                                  ref="Placebo",
                                  adjust="holm",
                                  level=1-alfa)
print(EMM.contrast)
print(plot(EMM.contrast,
           colors="black"))

cat(bartitle("Teste post hoc: Grupo (fator) ajustado por Experiencia (covariável): Consecutivo"))
EMM.contrast <- emmeans::contrast(EMM, 
                                  method="consec",
                                  adjust="holm",
                                  level=1-alfa)
print(EMM.contrast)
print(plot(EMM.contrast,
           colors="black"))
