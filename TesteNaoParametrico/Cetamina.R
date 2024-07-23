library(readxl)
library(lmerTest)
library(multcomp)
Dados.long <- readxl::read_excel("Cetamina_long.xlsx")
alfa <- 0.05

# GLMM
Dados.long$Paciente <- factor(Dados.long$Paciente)
Dados.long$Genero <- factor(Dados.long$Genero)
Dados.long$Teste <- factor(Dados.long$Teste)
Dados.long$Avaliacao <- factor(Dados.long$Avaliacao)
Dados.long <- Dados.long[Dados.long$Avaliacao!="ComCetamina3",]

Teste.rotulo <- factor(unique(Dados.long$Teste))
i <- 1
sink("Cetamina_GLMM.txt")
for(i in 1:length(Teste.rotulo)){
Dados.long.teste <- subset(Dados.long, Teste==Teste.rotulo[i])
cat(paste0("\nTeste neuropsicologico: ", Teste.rotulo[i]))
cat("\nGLMM: omnibus test\n")
res <- lmerTest::lmer(Escore ~ Avaliacao +
                               Idade + GrauInstrucao +
                               (1|Paciente), 
                      data=Dados.long.teste, 
                      REML=TRUE)
cat("\n") 
print(stats::anova(res)) 
cat("\n") 
cat("\nPost hoc test: Pairwise Contrasts\n")
EMM <- emmeans::emmeans(res, 
                        specs=pairwise~Avaliacao,
                        level=1-alfa,
                        adjust="tukey")
print(summary(EMM, 
              infer=TRUE)$contrasts)
}
sink()

# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
cat("\nEstimated Marginal Means")
EMM <- emmeans::emmeans(res, 
                        specs=pairwise~Avaliacao,
                        level=1-alfa,
                        adjust="tukey")
print(summary(EMM, 
              infer=TRUE)$emmeans)
print(plot(EMM, 
           colors="black",
           main="Estimated Marginal Means",
           xlab="Escore",
           ylab="Avaliacao"))

cat("\nPost hoc test: Pairwise Contrasts")
EMM <- emmeans::emmeans(res, 
                        specs=pairwise~Avaliacao,
                        level=1-alfa,
                        adjust="tukey")
print(summary(EMM, 
              infer=TRUE)$contrasts)
print(plot(confint(EMM$contrasts),
           colors="black"))

cat("\nPost hoc test: Reference Level Contrasts")
EMM <- emmeans::emmeans(res, 
                        specs=trt.vs.ctrl~Avaliacao,
                        ref=3,
                        reverse=FALSE,
                        level=1-alfa,
                        adjust="dunnett")

print(summary(EMM, 
              infer=TRUE)$contrasts)
print(plot(confint(EMM$contrasts),
           colors="black"))

mc <- multcomp::glht(res, 
                     linfct=mcp(Avaliacao="Tukey"),
                     alternative="two.sided")
mcs <- summary(mc, test=adjusted("bonferroni"))
print(mcs)
print(multcomp::cld(mcs, level=alfa, decreasing=TRUE))
plot(mc,las=3)
mc <- multcomp::glht(res, 
                     linfct=mcp(Avaliacao="Dunnett"),
                     alternative="two.sided")
mcs <- summary(mc, test=adjusted("bonferroni"))
print(mcs)
plot(mc,las=3)

Dados <- readxl::read_excel("Cetamina.xlsx")
# Teste t relacionado
out21 <- tapply(Dados$Aval2-Dados$Aval1, 
                Dados$Teste, 
                t.test,
                mu=0,
                alternative="two.sided")
print(out21)
print(out21$Animais)
print(out21[[1]])
print(out21$Animais$p.value)

out31 <- tapply(Dados$Aval3-Dados$Aval1, 
                Dados$Teste, 
                t.test,
                mu=0,
                alternative="two.sided")
print(out31)

out32 <- tapply(Dados$Aval3-Dados$Aval2, 
                Dados$Teste, 
                t.test,
                mu=0,
                alternative="two.sided")
print(out32)




