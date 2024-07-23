source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")

cat(bartitle("Teste do efeito do fator fixo",2))
## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat(bartitle("Igualdade dos interceptos das regressões\n(assumindo igualdade das inclinações)\n- Teste de efeito do fator -",3))
ancova.fit <- lm(ErrosDirecao~Grupo+Experiencia, 
                 data=Dados)
print(Anova <- car::Anova(ancova.fit))
print(summary(ancova.fit))
