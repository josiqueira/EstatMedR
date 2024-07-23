suppressMessages(library(readxl, warn.conflicts = FALSE))
suppressMessages(library(heplots, warn.conflicts = FALSE))

source("eiras.bartitle.R")
source("eiras.Eta2classification.R")

options(warn=-1) # disable warnings

print(iris)

cat(bartitle("MANOVA"))
fit <- lm(cbind(Estatura, MCT) ~ Genero, 
          data=Dados, na.action="na.omit")

cat(bartitle("ANOVA da MANOVA",2))
print(summary(car::Anova(fit)))
cat(bartitle("Effect size",3))
eta2 <- as.numeric(heplots::etasq(fit))
Eta2classification(eta2)

cat(bartitle("univariate ANOVAs",2))
fit <- lm(cbind(Estatura, MCT) ~ Genero, 
          data=Dados, na.action="na.omit")
print(summary(car::Anova(fit), 
              univariate=TRUE, 
              multivariate=FALSE,
              p.adjust.method=TRUE))
cat(bartitle("Estatura",3))
fitEstatura <- lm(Estatura ~ Genero, 
                  data=Dados, na.action="na.omit")
print(car::Anova(fitEstatura))
cat(bartitle("MCT",3))
fitMCT <- lm(MCT ~ Genero, 
                  data=Dados, na.action="na.omit")
print(car::Anova(fitMCT))
