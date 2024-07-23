# demo_ANOVA2f_premissas.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(lawstat))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

Dados <- readxl::read_excel("CafeinaAlcool_entre.xlsx")
Dados$Cafeina <- factor(Dados$Cafeina)
Dados$Alcool <- factor(Dados$Alcool)
Dados$UE <- factor(Dados$UE)

cat(bartitle("Data"))
showdataframe(Dados,4,3)


cat(bartitle("Assumptions"))
cat(bartitle("Symmetry",2))
cat(bartitle("by Alcool",3))
Alcool <- unique(Dados$Alcool)
for (i in 1:length(Alcool))
{
  cat("\nAlcool = ",as.character(Alcool[i]),"\n")
  print(lawstat::symmetry.test(Dados$NumErros[Dados$Alcool==Alcool[i]]))
}
cat(bartitle("by Cafeina",3))
Cafeina <- unique(Dados$Cafeina)
for (i in 1:length(Cafeina))
{
  cat("\nCafeina = ",as.character(Cafeina[i]),"\n")
  print(lawstat::symmetry.test(Dados$NumErros[Dados$Cafeina==Cafeina[i]]))
}

cat(bartitle("Normality",2))
cat(bartitle("by Alcool",3))
print(RcmdrMisc::normalityTest(NumErros ~ Alcool, data=Dados))
cat(bartitle("by Cafeina",3))
print(RcmdrMisc::normalityTest(NumErros ~ Cafeina, data=Dados))

cat(bartitle("Homoscedasticity",2))
cat(bartitle("by Alcool",3))
print(car::leveneTest(NumErros ~ Alcool, data=Dados, center=median))
cat(bartitle("by Cafeina",3))
print(car::leveneTest(NumErros ~ Cafeina, data=Dados, center=median))

# enable warnings
options(warn=0)
