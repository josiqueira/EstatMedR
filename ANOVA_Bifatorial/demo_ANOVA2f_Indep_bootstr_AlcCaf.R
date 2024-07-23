Dados <- readRDS("CafeinaAlcool_entre.rds")

# ANOVA multifatorial independente por por reamostragem (_bootstrapping_):   
bootsamples <- 1e3
modelo_boot <- lmboot::ANOVA.boot(NumErros ~ Cafeina*Alcool, 
                                  B=bootsamples, 
                                  type="residual", 
                                  wild.dist="normal",  
                                  seed=123,
                                  data=Dados, 
                                  keep.boot.resp=FALSE)
print(modelo_boot$`p-values`)

