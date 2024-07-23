source("eiras.bartitle.R")

options(warn=-1) # disable warnings

Dados <- readRDS("Adm2008.rds")

cat(bartitle("MANCOVA"))

fit <- manova(cbind(Estatura,MCT)~Sexo+Idade,
              data=Dados)
print(anv <- car::Anova(fit,
                        univariate=FALSE, 
                        multivariate=TRUE,
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

