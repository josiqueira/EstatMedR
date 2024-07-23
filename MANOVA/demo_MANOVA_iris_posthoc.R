# Testes post hoc
source("eiras.bartitle.R")
alpha <- 0.05
# setosa & versicolor
cat(bartitle("setosa & versicolor"))
Dados1 <- subset(iris, Species!="virginica")
fit <- manova(cbind(Sepal.Length, Sepal.Width, 
                    Petal.Length, Petal.Width) ~ Species, 
              data=Dados1)
print(anv <- car::Anova(fit, 
                        test="Pillai"), 
      digits=3)
eta2 <- effectsize::eta_squared(anv,
                                partial=TRUE,
                                generalized=FALSE,
                                ci=1-alpha,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=2)

# setosa & virginica
cat(bartitle("setosa & virginica"))
Dados2 <- subset(iris, Species!="versicolor")
fit <- manova(cbind(Sepal.Length, Sepal.Width, 
                    Petal.Length, Petal.Width) ~ Species, 
              data=Dados2)
print(anv <- car::Anova(fit, 
                        test="Pillai"), 
      digits=3)
eta2 <- effectsize::eta_squared(anv,
                                partial=TRUE,
                                generalized=FALSE,
                                ci=1-alpha,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=2)

# versicolor & virginica
cat(bartitle("versicolor & virginica"))
Dados3 <- subset(iris, Species!="setosa")
fit <- manova(cbind(Sepal.Length, Sepal.Width, 
                    Petal.Length, Petal.Width) ~ Species, 
              data=Dados3)
print(anv <- car::Anova(fit, 
                        test="Pillai"), 
      digits=3)
eta2 <- effectsize::eta_squared(anv,
                                partial=TRUE,
                                generalized=FALSE,
                                ci=1-alpha,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=2)
