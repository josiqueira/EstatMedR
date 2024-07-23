library(readxl)
suppressMessages(library(lmerTest, warn.conflicts = FALSE))
suppressMessages(library(multcomp, warn.conflicts = FALSE))

## Sachs, 1997, p. 675
## Six persons (block) received six different diuretics
## (A to F, treatment).
## The responses are the Na-concentration (mval)
## in the urine measured 2 hours after each treatment.
## Assume A is the control.
Data <- matrix(c(
  3.88, 5.64, 5.76, 4.25, 5.91, 4.33, 30.58, 30.14, 16.92,
  23.19, 26.74, 10.91, 25.24, 33.52, 25.45, 18.85, 20.45,
  26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 30.72,
  32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23,
  26.65),nrow=6, ncol=6,
  dimnames=list(1:6, LETTERS[1:6]))

# transformação em formato long
nameslong <- c("Paciente","Tratamento","Sodio")
Dados.long <- data.frame(matrix(nrow=0,ncol=length(nameslong)))
names(Dados.long) <- nameslong

for (r in 1:nrow(Data)) # pacientes
{
  tratamentos <- colnames(Data) # tratamentos
  sodios <- as.numeric(Data[r,]) # resultados do paciente
  tmp <- data.frame(rep(r,length(sodios)),tratamentos,sodios)
  names(tmp) <- nameslong
  Dados.long <- rbind(Dados.long,tmp)
}
Dados.long$Paciente <- factor(Dados.long$Paciente)
Dados.long$Tratamento <- factor(Dados.long$Tratamento)
print(Dados.long)

# GLMM
alfa <- 0.05

cat("\nGLMM: omnibus test\n")
res <- lmerTest::lmer(Sodio ~ Tratamento + (1|Paciente), 
                      data=Dados.long)
cat("\n") 
print(stats::anova(res)) 
cat("\n") 

cat("\nPost hoc test: Pairwise Contrasts\n")
EMM <- emmeans::emmeans(res, 
                        specs=pairwise~Tratamento,
                        level=1-alfa,
                        adjust="tukey")
print(plot(EMM, 
           colors="black",
           main="Estimated Marginal Means",
           xlab="Escore",
           ylab="Tratamento"))
print(summary(EMM, 
              infer=TRUE)$contrasts)
print(plot(confint(EMM$contrasts),
           colors="black"))

cat("\nPost hoc test: Reference Level Contrasts")
EMM <- emmeans::emmeans(res, 
                        specs=trt.vs.ctrl~Tratamento,
                        ref=1,
                        reverse=FALSE,
                        level=1-alfa,
                        adjust="dunnett")

print(summary(EMM, 
              infer=TRUE)$contrasts)
print(plot(confint(EMM$contrasts),
           colors="black"))

