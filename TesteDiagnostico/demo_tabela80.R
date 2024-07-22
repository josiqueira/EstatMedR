prevalencia <- 0.8 # probabilidade pre-exame dada pelo medico
sensibilidade <- 0.9036 
especificidade <- 0.8475 
n <- 1
alfa <- 0.05
tabela2x2 <- as.table(matrix(n*c(prevalencia*sensibilidade,       
                                 (1 - prevalencia)*(1 - especificidade),
                                 prevalencia*(1 - sensibilidade), 
                                 (1 - prevalencia)*especificidade), 
                             nrow = 2, byrow = TRUE))
prmatrix(tabela2x2)
out <- epiR::epi.tests(tabela2x2, conf.level = 1-alfa)
sumario <- summary(out)
print(out)
print(sumario, digits=3)
