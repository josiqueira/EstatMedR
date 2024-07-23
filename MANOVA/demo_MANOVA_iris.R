source("eiras.bartitle.R")
options(warn=-1) # disable warnings

cat(bartitle("MANOVA"))
cat(bartitle("Tabela ANOVA da MANOVA",2))
fit <- manova(cbind(Sepal.Length,Sepal.Width,
                    Petal.Length,Petal.Width) ~ Species, 
              data=iris)
print(anv <- car::Anova(fit, 
                        test="Pillai"), 
      digits=3)
alpha <- 0.05
eta2 <- effectsize::eta_squared(anv,
                                partial=TRUE,
                                generalized=FALSE,
                                ci=1-alpha,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=2)

