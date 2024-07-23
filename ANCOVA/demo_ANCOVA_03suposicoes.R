source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")

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
