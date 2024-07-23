# demo_ANOVA2f_indep.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as variaveis

suppressMessages(library(readxl))
suppressMessages(library(MuMIn))
suppressMessages(library(heplots))

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

# ANOVA da ANOVA
modelo <- lm(NumErros ~ Alcool*Cafeina, data=Dados)
cat(bartitle("ANOVA",2))
print(anv <- car::Anova(modelo, type=2, white.adjust=TRUE))

cat(bartitle("Effect size analysis"))
cat(bartitle("Omnibus",2))
eta2 <- as.numeric(MuMIn::r.squaredLR(modelo))
Eta2classification(eta2)
cat(bartitle("Partials",2))
eta2p <- heplots::etasq(modelo)
eta2p$classification <- NA
for (r in 1:nrow(eta2p))
{
  if(!is.na(eta2p$`Partial eta^2`[r]))
  {
    eta2p$classification[r] <- Eta2classification(eta2p$`Partial eta^2`[r],show=FALSE)  
  }
}
eta2p <- eta2p[!is.na(eta2p$`Partial eta^2`),]
prmatrix(eta2p,quote=FALSE)

# enable warnings
options(warn=0)
