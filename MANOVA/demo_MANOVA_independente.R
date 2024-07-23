source("eiras.bartitle.R")

options(warn=-1) # disable warnings

cat(bartitle("Data"))
Dados <- readRDS("Adm2008.rds")
Dados <- Dados[, c("Sexo","Estatura","MCT")]

cat(bartitle("T^2 de Hotelling (homocedástico)"))
# Solução 1: DescTools::HotellingsT2Test
# T.2 é F
print(DescTools::HotellingsT2Test(cbind(Estatura,MCT)~Sexo,
                                  data = Dados))

cat(bartitle("MANOVA (homocedástica)"))
# Solução 2: manova
fit <- manova(cbind(Estatura,MCT)~Sexo,
              data = Dados)
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
