source("eiras.bartitle.R")
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
alfa <- 0.05
Dados.long <- readRDS("PrePos_2CondIndep.rds")$Dados.long
Dados.wide <- readRDS("PrePos_2CondIndep.rds")$Dados.wide

cat(bartitle("Data"))
print.data.frame(Dados.long)

cat(bartitle("Descriptive Statistics"))
print(ftable(Dados.long[,c("Grupo","Momento","Paciente")]))
print(psych::describeBy(Escore~Momento*Grupo,
                        mat=1,
                        digits=2,
                        data=Dados.long))
boxplot(Escore~Momento*Grupo, data=Dados.long, ylab="Escore")
print(GGally::ggpairs(Dados.wide[-1], ggplot2::aes(colour=Grupo)))

cat(bartitle("Simple linear regressions"))

# formato wide eh melhor
car::scatterplot(Pos~Pre|Grupo, data=Dados.wide, 
                 regLine=TRUE, smooth=FALSE, col="black")
# o mesmo com os dados no formato long
# car::scatterplot(Escore[Momento=="Pos"]~Escore[Momento=="Pre"]|Grupo[Momento=="Pre"], data=Dados.long,
#                  regLine=TRUE, smooth=FALSE, col="black")

# testes usam o formato wide
graf <- ggplot2::ggplot(Dados.wide, 
                        ggplot2::aes(y=Pos, 
                                     x=Pre, 
                                     group=Grupo, 
                                     linetype=Grupo,
                                     shape=Grupo))+ 
  ggplot2::geom_point(alpha=1/4,
                      size=1)+ 
  ggplot2::theme_classic()+
  ggplot2::labs(title="RLS por Grupo",
                subtitle=paste0("BC",
                                round((1-alfa/
                                         length(unique(Dados.wide$Grupo))),4)*
                                  100,"%"),
                x="Escore Pre", 
                y="Escore Pos")+
  ggplot2::geom_smooth(method=MASS::rlm,
                       na.rm=TRUE,
                       color="black",
                       level=1-alfa/length(unique(Dados.wide$Grupo)))
plot(graf)

for (g in unique(Dados.wide$Grupo))
{
  cat(bartitle(g,3))
  dt_tmp <- subset(Dados.wide, Grupo==g)
  modelo <- lm(Pos~Pre, data=dt_tmp)
  print(summary(modelo))
}

cat(bartitle("ANCOVA"))

cat(bartitle("Assumptions",2))

## ANCOVA: Teste de dissociacao entre fator e covariavel
cat(bartitle("Dissociation between Grupo (factor) and VD Pre (covariate)",3))
modelo <- lm(Pre ~ Grupo, 
             data=Dados.wide)
print(Anova <- car::Anova(modelo))

## ANCOVA: Teste de linearidade da VD e covariavel nos niveis do fator
cat(bartitle("Linearity between Escore pos (VD) and Escore pre (covariate)",3))
levels <- as.vector(unique(Dados.wide$Grupo))
for (l in levels)
{
  modelo <- lm(Pos ~ Pre, 
               data=Dados.wide[Dados.wide$Grupo==l,])
  hc <- lmtest::harvtest(modelo) # Harvey-Collier test for linearity
  cat(bartitle(l,4))
  print(hc)
}

## ANCOVA: Teste de igualdade das inclinacoes das retas de regressao
cat(bartitle("Homogenous slope of regressions",3))
modelo <- lm(Pos ~ Grupo + Pre + Grupo:Pre, 
             data=Dados.wide)
print(Anova <- car::Anova(modelo))

## ANCOVA: Teste do efeito do fator fixo: 
##          Se as declividades sao iguais, testar se os interceptos sao iguais.
cat(bartitle("Homogenous intercepts of regressions\n(assuming homogeneous slopes)\n- Factor Effect Test -",3))
ancova.fit <- lm(Pos~Grupo+Pre, 
                 data=Dados.wide)
print(Anova <- car::Anova(ancova.fit))
print(summary(ancova.fit))

cat(bartitle("Effect size analysis"))
cat(bartitle("Omnibus",2))
eta2 <- summary(ancova.fit)$r.squared
cat("R^2 = eta^2 omnibus = ", round(eta2,2), "\n", sep="")
print(effectsize::interpret_eta_squared(eta2))
eta2 <- effectsize::eta_squared(Anova,
                                partial = TRUE,
                                generalized = FALSE,
                                ci = 1-alfa,
                                alternative = "two.sided",
                                verbose = TRUE)
print(eta2, digits = 2)
es <- effectsize::interpret_eta_squared(eta2$Eta2)
names(es) <- c("Tamanho de efeito: estimativa pontual")
print(es)

cat(bartitle("Adjusted estimated marginal means"))

EMM <- emmeans::emmeans(ancova.fit, 
                        pairwise~"Grupo", 
                        adjust="holm",
                        level=1-alfa)
print(EMM$emmeans)
print(plot(EMM$emmeans,
           colors="black", 
           main="Adjusted Estimated Marginal Means",
           xlab="VD Pós controlada por VD Pré",
           xlim=c(4,12),
           ylab="Grupo"))

cat(bartitle("Post hoc tests: Grupo (factor) adjusted by VD Pre (covariate)"))

print(summary(EMM$contrasts, infer=TRUE))
print(plot(EMM$contrasts, 
           colors="black"))
print(multcomp::cld(object=EMM$emmeans,
                    level=1-alfa,
                    adjust="holm",
                    Letters=letters,
                    alpha=alfa))
