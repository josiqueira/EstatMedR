# demo_ANOVA2f_interacao.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(phia))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

alfa <- 0.05
Dados <- readxl::read_excel("CafeinaAlcool_entre.xlsx")
Dados$Cafeina <- factor(Dados$Cafeina)
Dados$Alcool <- factor(Dados$Alcool)
Dados$UE <- factor(Dados$UE)

cat(bartitle("Simple Main Effects"))
modelo <- lm(NumErros ~ Alcool*Cafeina, data=Dados)

cat("\n- Alcool at each level of Cafeina:\n")
print(phia::testInteractions(modelo, fixed="Cafeina", across="Alcool"))

cat("\n- Cafeina at each level of Alcool:\n")
print(phia::testInteractions(modelo, fixed="Alcool", across="Cafeina"))

# Grafico
o.par <- par()
fit.means <- phia::interactionMeans(modelo)
plot(fit.means, errorbar=paste("ci",round((1-alfa)*100,0),sep=""))
par(o.par)

# enable warnings
options(warn=0)
