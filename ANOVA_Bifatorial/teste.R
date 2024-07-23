# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(stats))
suppressMessages(library(lmerTest))
suppressMessages(library(car))
suppressMessages(library(MuMIn))
suppressMessages(library(emmeans))
suppressMessages(library(phia))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")
source("eiras_plotIC.R")
source("eiras.showdataframe.R")

# suppress warnings
options(warn=-1)

alfa <- 0.05

# Dados <- read_excel("CafeinaAlcool_entre.xlsx")
# Dados <- read_excel("CafeinaAlcool_intra.xlsx")
# Dados <- read_excel("Cafeinaintra_Alcoolentre.xlsx")
Dados$Cafeina <- factor(Dados$Cafeina)
Dados$Alcool <- factor(Dados$Alcool)
Dados$UE <- factor(Dados$UE)

cat(bartitle("Data"))
showdataframe(Dados)
cat("\n\tFactors (Cafeina & Alcool):\n")
print(xtabs(~Alcool+Cafeina, data=Dados))

cat(bartitle("Repeated Measures Two-way ANOVA"))

suppressMessages(
modelo <- lmerTest::lmer(NumErros ~ Alcool*Cafeina + (1|UE), 
                         data=Dados)
)
# cat(bartitle("Regression",2))
# print(summary(modelo))

cat(bartitle("ANOVA",2))
print(anv <- car::Anova(modelo))

cat(bartitle("Effect size analysis"))
eta2 <- as.numeric(MuMIn::r.squaredGLMM(modelo))[1]
Eta2classification(eta2)

cat(bartitle("Simple main effects"))
cat("\n\t- Alcool at Cafeina levels\n")
print(phia::testInteractions(modelo, fixed="Cafeina", across="Alcool"))
cat("\n\t- Cafeina at Alcool levels\n")
print(phia::testInteractions(modelo, fixed="Alcool", across="Cafeina"))

# Grafico
o.par <- par()
fit.means <- phia::interactionMeans(modelo)
plot(fit.means, errorbar=paste("ci",round((1-alfa)*100,0),sep=""))
par(o.par)

cat(bartitle("Post hoc tests"))

cat(bartitle("Alcool",2))
print(EMM.A <- emmeans::emmeans(modelo, 
                                pairwise~"Alcool", 
                                level=1-alfa))
print(plot(EMM.A$emmeans,colors="black",xlab="NumErros"))

cat(bartitle("Cafeina",2))
print(EMM.B <- emmeans::emmeans(modelo, 
                                pairwise~"Cafeina", 
                                level=1-alfa))
print(plot(EMM.B$emmeans,colors="black",xlab="NumErros"))

cat(bartitle("Alcool:Cafeina",2))
print(EMM.AB <- emmeans::emmeans(modelo, 
                                 pairwise~"Alcool:Cafeina",
                                 level=1-alfa))
print(plot(EMM.AB$emmeans,colors="black",xlab="NumErros"))

outdif <- lmerTest::difflsmeans(modelo,ddf = c("Kenward-Roger"),level=alfa)
print(outdif)
plot(outdif)

# enable warnings
options(warn=0)
