suppressMessages(library(multcomp))
suppressMessages(library(readxl))

source("eiras.bartitle.R")
source("eiras_plotIC.R")

# suppress warnings
options(warn=-1)

Dados <- read.csv("Brainstorming20_5Kutner.dat", sep="\t")
Dados$Profissional <- factor(Dados$Profissional)
Dados$TamanhoGrupo <- factor(Dados$TamanhoGrupo)
n.f1 <- length(unique(Dados$Profissional))
n.f2 <- length(unique(Dados$TamanhoGrupo))

alfa<-0.05
modelo <- lm(Ideias~Profissional+TamanhoGrupo, data=Dados)
res <- car::Anova(modelo, type=2, white.adjust=TRUE)

cat(bartitle("Post-hoc tests"))

fatores <- unique(as.character(Dados$Profissional))
letra <- "A"
legenda <- c()
cat ("\nLegenda:\n")
Dados$ProfissionalShort <- NA
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  Dados$ProfissionalShort[Dados$Profissional==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
Dados$ProfissionalShort <- as.factor(Dados$ProfissionalShort)
modelo2 <- lm(Ideias~ProfissionalShort+TamanhoGrupo, data=Dados)
mc.tukey <- multcomp::glht(modelo2, linfct = multcomp::mcp(ProfissionalShort = "Tukey"))
print(mcs.tukey <- summary(mc.tukey, test=adjusted("bonferroni")))
multcomp::cld(mcs.tukey, level=alfa, decreasing=TRUE)
t.ic <- mcs.tukey$test$qfunction(0.95)
df_plot <- data.frame(
  names (mcs.tukey$test$coefficient),
  as.vector(mcs.tukey$test$coefficients),
  as.vector(mcs.tukey$test$coefficients)-t.ic*as.vector(mcs.tukey$test$sigma),
  as.vector(mcs.tukey$test$coefficients)+t.ic*as.vector(mcs.tukey$test$sigma),
  as.vector(mcs.tukey$test$pvalues)
)
eiras_plotIC(df_plot,
             main="95% family-wise confidence level",
             xlab="Difference",
             usecolor="n"
)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")

mc.dunnett <- multcomp::glht(modelo2, linfct = multcomp::mcp(ProfissionalShort = "Dunnett"))
mcs.dunnett <- summary(mc.dunnett, test=adjusted("bonferroni"))
print(mcs.dunnett)
t.ic <- mcs.dunnett$test$qfunction(0.95)
df_plot <- data.frame(
  names (mcs.dunnett$test$coefficient),
  as.vector(mcs.dunnett$test$coefficients),
  as.vector(mcs.dunnett$test$coefficients)-t.ic*as.vector(mcs.dunnett$test$sigma),
  as.vector(mcs.dunnett$test$coefficients)+t.ic*as.vector(mcs.dunnett$test$sigma),
  as.vector(mcs.dunnett$test$pvalues)
)
eiras_plotIC(df_plot,
             main="95% family-wise confidence level",
             xlab="Difference",
             usecolor="n"
)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")


fatores <- unique(as.character(Dados$TamanhoGrupo))
letra <- "A"
legenda <- c()
cat ("\nLegenda:\n")
Dados$TamanhoGrupoShort <- NA
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  Dados$TamanhoGrupoShort[Dados$TamanhoGrupo==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
Dados$TamanhoGrupoShort <- as.factor(Dados$TamanhoGrupoShort)
modelo2 <- lm(Ideias~TamanhoGrupoShort+Profissional, data=Dados)
mc.tukey <- multcomp::glht(modelo2, linfct = multcomp::mcp(TamanhoGrupoShort = "Tukey"))
print(mcs.tukey <- summary(mc.tukey, test=adjusted("bonferroni")))
print(multcomp::cld(mcs.tukey, level=alfa, decreasing=TRUE))
t.ic <- mcs.tukey$test$qfunction(0.95)
df_plot <- data.frame(
  names (mcs.tukey$test$coefficient),
  as.vector(mcs.tukey$test$coefficients),
  as.vector(mcs.tukey$test$coefficients)-t.ic*as.vector(mcs.tukey$test$sigma),
  as.vector(mcs.tukey$test$coefficients)+t.ic*as.vector(mcs.tukey$test$sigma),
  as.vector(mcs.tukey$test$pvalues)
)
eiras_plotIC(df_plot,
             main="95% family-wise confidence level",
             xlab="Difference",
             usecolor="n"
)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")

mc.dunnett <- multcomp::glht(modelo2, linfct = multcomp::mcp(TamanhoGrupoShort = "Dunnett"))
mcs.dunnett <- summary(mc.dunnett, test=adjusted("bonferroni"))
print(mcs.dunnett)
t.ic <- mcs.dunnett$test$qfunction(0.95)
df_plot <- data.frame(
  names (mcs.dunnett$test$coefficient),
  as.vector(mcs.dunnett$test$coefficients),
  as.vector(mcs.dunnett$test$coefficients)-t.ic*as.vector(mcs.dunnett$test$sigma),
  as.vector(mcs.dunnett$test$coefficients)+t.ic*as.vector(mcs.dunnett$test$sigma),
  as.vector(mcs.dunnett$test$pvalues)
)
eiras_plotIC(df_plot,
             main="95% family-wise confidence level",
             xlab="Difference",
             usecolor="n"
)
legend("topleft",legenda,lwd=0,lty=0,cex=0.6,box.lwd=0, border="transparent", bg="transparent")


# enable warnings
options(warn=0)
