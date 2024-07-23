source("eiras.bartitle.R")
Dados <- readRDS("AlcoholExperienceDrivingErrors.rds")
alfa <- 0.05
cat(bartitle("Análise de tamanho de efeito",2))
cat(bartitle("Omnibus",2))
eta2 <- summary(ancova.fit)$r.squared
cat("R^2 = eta^2 omnibus = ", round(eta2,2), "\n", sep="")
print(effectsize::interpret_eta_squared(eta2))
eta2 <- effectsize::eta_squared(Anova,
                                partial=FALSE,
                                generalized=FALSE,
                                ci=1-alfa,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=2)