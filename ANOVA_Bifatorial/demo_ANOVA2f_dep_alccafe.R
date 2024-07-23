source("summarySEwithin2.R")
Dados <- readRDS("CafeinaAlcool_intra.rds")
alfa <- 0.05
print(ftable(Dados[,-4]))
print(psych::describe(NumErros ~ Alcool*Cafeina, 
                      data=Dados), digits=2)
alfaBonf <- alfa/(length(unique(Dados$Alcool))*
                  length(unique(Dados$Cafeina)))
ic <- summarySEwithin2(Dados,
                       measurevar="NumErros",
                       withinvars=c("Alcool","Cafeina"),
                       idvar="UE",
                       na.rm=TRUE,
                       conf.interval=1-alfaBonf)
print(ic)
grf <- ggplot2::ggplot(ic,
                       ggplot2::aes(x=Alcool,
                                    y=NumErros,
                                    colour=Cafeina)) +
  ggplot2::geom_errorbar(position=ggplot2::position_dodge(.9),
                         width=.1,
                         ggplot2::aes(ymin=NumErros-ci,
                                      ymax=NumErros+ci)) +
  ggplot2::geom_point(shape=21,
                      size=3,
                      fill="white",
                      position=ggplot2::position_dodge(.9)) +
  ggplot2::ylab("NumErros") +
  ggplot2::ggtitle("Álcool & Cafeína: Número de Erros\nWithin-subject CI95% Bonferroni") +
  ggplot2::theme_bw()
print(grf)

modelo <- lmerTest::lmer(NumErros ~ Alcool*Cafeina + (1|UE), 
                         data=Dados)
cat("\nANOVA")
print(anv <- car::Anova(modelo,test.statistic="F"))
print(summary(modelo, correl=FALSE))

cat("\nEffect size analysis")
eta2g <- as.numeric(MuMIn::r.squaredGLMM(modelo)[1])
cat("\nTamanho de efeito: eta^2 omnibus =", eta2g)
es <- effectsize::interpret_eta_squared(eta2g)
names(es) <- c("Tamanho de efeito omnibus: estimativa pontual")
print(es)
eta2 <- effectsize::eta_squared(anv,
                                partial=FALSE,
                                generalized=FALSE,
                                ci=1-alfa/3,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=6)

# Grafico de perfil de médias
o.par <- par()
fit.means <- phia::interactionMeans(modelo)
plot(fit.means, 
     errorbar=paste0("ci",
                     round((1-alfaBonf)*100,4)),
     abbrev.levels=TRUE)
par(o.par)

plot(effects::effect(c("Cafeina"), modelo, confidence.level=1-alfa/
                       length(unique(Dados$Cafeina))), 
     ci.style = "bars")
plot(effects::effect(c("Alcool"), modelo, confidence.level=1-alfa/
                       length(unique(Dados$Alcool))), 
     ci.style = "bars")
plot(effects::effect(c("Cafeina", "Alcool"), modelo, 
                     confidence.level=1-alfaBonf), 
     multiline = TRUE, ci.style = "bars")
plot(effects::allEffects(modelo, 
                         confidence.level=1-alfaBonf), 
     multiline = TRUE, ci.style = "bars")

cat("\nSimple main effects: global")
cat("\n\t- Alcool at Cafeina levels\n")
print(phia::testInteractions(modelo, fixed="Cafeina", across="Alcool"))
cat("\n\t- Cafeina at Alcool levels\n")
print(phia::testInteractions(modelo, fixed="Alcool", across="Cafeina"))

# cat("\nSimple main effects: pairwise")
# cat("\n\t- Alcool at Cafeina levels\n")
# EMM.AatC <- emmeans::emmeans(modelo, 
#                              specs=pairwise~"Alcool|Cafeina",
#                              adjust="holm",
#                              level=1-alfa,
#                              lmer.df="satterthwaite",
#                              lmerTest.limit=nrow(Dados))
# print(summary(EMM.AatC$contrasts, infer=TRUE))
# print(plot(EMM.AatC$contrasts, 
#            colors="black"))
# print(multcomp::cld(object=EMM.AatC$emmeans,
#                     level=1-alfa,
#                     adjust="holm",
#                     Letters=letters,
#                     alpha=alfa))
# cat("\n\t- Cafeina at Alcool levels\n")
# EMM.CatA <- emmeans::emmeans(modelo, 
#                              specs=pairwise~"Cafeina|Alcool",
#                              adjust="holm",
#                              level=1-alfa,
#                              lmer.df="satterthwaite",
#                              lmerTest.limit=nrow(Dados))
# print(summary(EMM.CatA$contrasts, infer=TRUE))
# print(plot(EMM.CatA$contrasts, 
#            colors="black"))
# print(multcomp::cld(object=EMM.CatA$emmeans,
#                     level=1-alfa,
#                     adjust="holm",
#                     Letters=letters,
#                     alpha=alfa))

cat("\nPost hoc tests")
cat("\n\tAlcool:Cafeina")
EMM.AC <- emmeans::emmeans(modelo, 
                           specs=pairwise~"Alcool:Cafeina",
                           adjust="holm",
                           level=1-alfa,
                           lmer.df="satterthwaite",
                           lmerTest.limit=nrow(Dados))
print(summary(EMM.AC$emmeans))
print(summary(EMM.AC$contrasts, infer=TRUE))
print(plot(EMM.AC$emmeans, 
           colors="black"))
print(plot(EMM.AC$contrasts, 
           colors="black"))
print(multcomp::cld(object=EMM.AC$emmeans,
                    level=1-alfa,
                    adjust="holm",
                    Letters=letters,
                    alpha=alfa))

cat("\n\tAlcool")
EMM.A <- emmeans::emmeans(modelo, 
                          specs=pairwise~"Alcool", 
                          adjust="holm",
                          level=1-alfa,
                          lmer.df="satterthwaite",
                          lmerTest.limit=nrow(Dados))
print(summary(EMM.A$emmeans))
print(summary(EMM.A$contrasts, infer=TRUE))
print(plot(EMM.A$emmeans, 
           colors="black"))
print(plot(EMM.A$contrasts, 
           colors="black"))
print(multcomp::cld(object=EMM.A$emmeans,
                    level=1-alfa,
                    adjust="holm",
                    Letters=letters,
                    alpha=alfa))

cat("\n\tCafeina")
EMM.C <- emmeans::emmeans(modelo, 
                          specs=pairwise~"Cafeina", 
                          adjust="holm",
                          level=1-alfa,
                          lmer.df="satterthwaite",
                          lmerTest.limit=nrow(Dados))
print(summary(EMM.C$emmeans))
print(summary(EMM.C$contrasts, infer=TRUE))
print(plot(EMM.C$emmeans, 
           colors="black"))
print(plot(EMM.C$contrasts, 
           colors="black"))
print(multcomp::cld(object=EMM.C$emmeans,
                    level=1-alfa,
                    adjust="holm",
                    Letters=letters,
                    alpha=alfa))

# outdif <- lmerTest::difflsmeans(modelo,
#                                 ddf=c("Kenward-Roger"),
#                                 level=alfa)
# print(outdif)
# plot(outdif)
