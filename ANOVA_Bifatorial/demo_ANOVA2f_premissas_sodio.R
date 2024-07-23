# demo_ANOVA2f_premissas.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))

suppressMessages(library(lawstat))
suppressMessages(library(MuMIn))
suppressMessages(library(emmeans))
suppressMessages(library(multcomp))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

Dados <- read_excel("Nutricao2fatores.xlsx")
Dados$Instructor <- factor(Dados$Instructor)
Dados$Student <- factor(Dados$Student)
Dados$Supplement <- factor(Dados$Supplement)

cat(bartitle("Data"))
showdataframe(Dados,4,3)


cat(bartitle("Assumptions"))

cat("\n-Symmetry:\n")
cat("\n\t-by Instructor:\n")
instructor <- unique(Dados$Instructor)
for (i in 1:length(instructor))
{
  cat("\nInstructor = ",as.character(instructor[i]),"\n")
  print(lawstat::symmetry.test(Dados$Sodium[Dados$Instructor==instructor[i]]))
}
cat("\n\t-by Supplement:\n")
supplement <- unique(Dados$Supplement)
for (i in 1:length(supplement))
{
  cat("\nSupplement = ",as.character(supplement[i]),"\n")
  print(lawstat::symmetry.test(Dados$Sodium[Dados$Supplement==supplement[i]]))
}

cat("\n-Normality:\n")
cat("\n\t-by Instructor:\n")
print(RcmdrMisc::normalityTest(Sodium ~ Instructor, data=Dados))
cat("\n\t-by Supplement:\n")
print(RcmdrMisc::normalityTest(Sodium ~ Supplement, data=Dados))

cat("\n-Homoscedasticity:\n\n")
cat("\n\t-by Instructor:\n")
print(car::leveneTest(Sodium ~ Instructor, data=Dados, center=median))
cat("\n\t-by Supplement:\n")
print(car::leveneTest(Sodium ~ Supplement, data=Dados, center=median))
