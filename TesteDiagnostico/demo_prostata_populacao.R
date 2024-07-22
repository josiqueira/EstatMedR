prevalencia <- 0.052 # probabilidade para homens com 70+ anos
sensibilidade <- 0.349 
especificidade <- 0.631 
n <- 1
alfa <- 0.05
tabela2x2 <- as.table(matrix(n*c(prevalencia*sensibilidade,       
                                 (1 - prevalencia)*(1 - especificidade),
                                 prevalencia*(1 - sensibilidade), 
                                 (1 - prevalencia)*especificidade), 
                             nrow = 2, byrow = TRUE))
out <- epiR::epi.tests(tabela2x2, conf.level = 1-alfa)
sumario <- summary(out)
print(sumario, digits=3)
