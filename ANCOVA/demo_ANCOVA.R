source("eiras.bartitle.R")
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")
alfa <- 0.05

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

cat(bartitle("Regressão Linear Simples"))

car::scatterplot(ErrosDirecao~Experiencia|Grupo, 
                 data=Dados,
                 regLine=TRUE, smooth=FALSE, col=c("black","blue","red"))

graf <- ggplot2::ggplot(Dados, 
                        ggplot2::aes(y=ErrosDirecao, 
                                     x=Experiencia, 
                                     group=Grupo, 
                                     linetype=Grupo,
                                     shape=Grupo))+ 
  ggplot2::geom_point(alpha=1/4,
                      size=1)+ 
  ggplot2::theme_classic()+
  ggplot2::labs(title="RLS por Grupo",
                subtitle=paste0("BC",round((1-alfa/length(unique(Dados$Grupo)))*100,2),"%"),
                x="Experiência (ano)", 
                y="#ErrosDirecao")+
  ggplot2::geom_smooth(method=MASS::rlm,
                       na.rm=TRUE,
                       color="black",
                       level = 1-alfa/length(unique(Dados$Grupo)))
plot(graf)
for (g in unique(Dados$Grupo))
{
  cat(bartitle(g,2))
  dt_tmp <- subset(Dados, Grupo==g)
  modelo <- lm(ErrosDirecao~Experiencia, data=dt_tmp)
  print(summary(modelo))
}

cat(bartitle("ANCOVA"))

cat(bartitle("Suposições",2))

## ANCOVA: Teste de dissociacao entre fator e covariavel
cat(bartitle("Dissociação entre Grupo (fator) e Experiencia (covariável)",3))
modelo <- lm(Experiencia ~ Grupo, data=Dados)
suppressMessages(
  print(Anova <- car::Anova(modelo))
)

## ANCOVA: Teste de linearidade da VD e covariavel nos niveis do fator
cat(bartitle("Linearidade entre ErrosDirecao (VD) e Experiencia (covariável)",3))
levels <- as.vector(unique(Dados$Grupo))
for (l in levels)
{
  modelo <- lm(ErrosDirecao ~ Experiencia, data=Dados[Dados$Grupo==l,])
  hc <- lmtest::harvtest(modelo) # Harvey-Collier test for linearity
  cat(bartitle(l,4))
  print(hc)
}

## ANCOVA: Teste de igualdade das inclinacoes das retas de regressao
cat(bartitle("Igualdade das inclinações das regressões",3))
modelo <- lm(ErrosDirecao ~ Grupo + Experiencia + Grupo:Experiencia, 
             data=Dados)
suppressMessages(
  print(Anova <- car::Anova(modelo))
)

cat(bartitle("Teste do efeito do fator fixo",2))
## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat(bartitle("Igualdade dos interceptos das regressões\n(assumindo igualdade das inclinações)\n- Teste de efeito do fator -",3))
ancova.fit <- lm(ErrosDirecao~Grupo+Experiencia, 
                 data=Dados)
print(Anova <- car::Anova(ancova.fit))
print(summary(ancova.fit))

cat(bartitle("Análise de tamanho de efeito",2))
cat(bartitle("Omnibus",2))
eta2 <- summary(ancova.fit)$r.squared
cat("R^2 = eta^2 omnibus = ", round(eta2,2), "\n", sep="")
print(effectsize::interpret_eta_squared(eta2))
eta2 <- effectsize::eta_squared(Anova,
                                partial=FALSE,
                                generalized=FALSE,
                                ci=1-alfa,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=2)

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

