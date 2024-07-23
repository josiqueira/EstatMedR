alfa <- 0.05

Dados <- readRDS("CafeinaAlcool_entre.rds")

print(ftable(Dados[,-4]))
cat("\nDescriptive Statistics")
cat("\nEstratification by Alcool")
print(psych::describeBy(NumErros~Alcool,
                        digits=2,
                        data=Dados))
cat("\nEstratification by Cafeina")
print(psych::describeBy(NumErros~Cafeina,
                        digits=2,
                        data=Dados))
cat("\nEstratification by Alcool and Cafeina")
print(psych::describeBy(NumErros~Alcool+Cafeina,
                        digits=2,
                        data=Dados))

boxplot(NumErros~Cafeina+Alcool, 
        data=Dados,
        ylab="NumErros")
alfaBonf <- alfa/(length(unique(Dados$Cafeina))*
                  length(unique(Dados$Alcool)))
gplots::plotmeans(NumErros~interaction(Alcool,Cafeina),
                  data=Dados,
                  p=1-alfaBonf,
                  main=paste("CI",round((1-alfaBonf)*100,4),"%",sep=""),
                  barcol="black",
                  connect=FALSE,)

cat("\nAssumptions")
cat("\nNormality")
norm.test <- by(data=Dados$NumErros, 
                INDICES=list("Alcool" = Dados$Alcool, 
                     "Cafeina" = Dados$Cafeina), 
                FUN=shapiro.test)
print(norm.test)

cat("\nHomoscedasticity")
homoc.test <- car::leveneTest(NumErros~Alcool*Cafeina,
                              data=Dados)
print(homoc.test)

cat("\nFisher's Bifatorial independent ANOVA")

# ANOVA bifatorial independente de Fisher
  
cat("\nStatistical analysis: omnibus test")
# teste omnibus: `lm`, `car::Anova`
modelo <- lm(NumErros ~ Alcool*Cafeina,  
             data=Dados)
cat("\nANOVA")
print(anv <- car::Anova(modelo))
print(reg <- summary(modelo))
cat("R^2 = eta^2 omnibus", reg$r.squared)
print(effectsize::interpret_eta_squared(reg$r.squared))
eta2 <- effectsize::eta_squared(anv,
                                partial=FALSE,
                                generalized=FALSE,
                                ci=1-alfa/3,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=6)

# grafico do teste F
fobs <- reg$fstatistic[1]
dfn <- reg$fstatistic[2]
dfd <- reg$fstatistic[3]
fc <- qf(1-alfa, dfn, dfd)
p <- 1-pf(fobs, dfn, dfd)
if (p < 1e-4)
{
  p <- sprintf("%.2e",p)
} else
{
  p <- sprintf("%.4f",p)
}
f <- seq(0,1.4*max(fc,fobs),length.out=300)
densf <- df(f, dfn, dfd)
plot(f, densf, 
     main="Fisher Two-way ANOVA: Omnibus test",
     xlab="F", ylab="Density", 
     lwd=2, type="l")
abline(v=fc, lty=3)
abline(v=fobs, lty=4)
legend("topright",
       c("H0: there is not model",
         paste("F(",dfn,",",dfd,",",1-alfa,") = ",round(fc,3),sep=""),
         paste("F(",dfn,",",dfd,") = ",round(fobs,3),"\n",
               "p = ",p,sep="")
       ),
       lwd=c(2,1,1), lty=c(1,3,4),
       cex=0.8, 
       bty="n")

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
#                              level=1-alfa)
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
#                              level=1-alfa)
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
                           level=1-alfa)
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
                          level=1-alfa)
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
                          level=1-alfa)
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






