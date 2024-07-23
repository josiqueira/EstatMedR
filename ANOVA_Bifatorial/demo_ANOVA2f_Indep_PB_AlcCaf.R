alfa <- 0.05

Dados <- readRDS("CafeinaAlcool_entre.rds")

# ANOVA multifatorial independente por _Parametric Bootstrap based Generalized Test_:   
omni_Caf <- twowaytests::gpTwoWay(NumErros ~ Cafeina*Alcool,
                                  alpha = alfa,
                                  data=Dados)
# Teste de efeito simples de Cafeina
twowaytests::paircompTwoWay(omni_Caf,
                            alpha = alfa,
                            adjust.method = "holm")
omni_Alc <- twowaytests::gpTwoWay(NumErros ~ Alcool*Cafeina, 
                                  alpha = alfa,
                                  data=Dados)
# Teste de efeito simples de Alcool
twowaytests::paircompTwoWay(omni_Alc,
                            alpha = alfa,
                            adjust.method = "holm")

