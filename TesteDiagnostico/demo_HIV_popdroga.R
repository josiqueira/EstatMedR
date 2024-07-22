prevalencia <- 0.05 # prevalencia para usuarios de droga
sensibilidade <- 0.997 
especificidade <- 0.985 
n <- 1
alfa <- 0.05
tabela2x2 <- as.table(matrix(n*c(prevalencia*sensibilidade,       
                                 (1 - prevalencia)*(1 - especificidade),
                                 prevalencia*(1 - sensibilidade), 
                                 (1 - prevalencia)*especificidade), 
                             nrow = 2, byrow = TRUE))
out <- epiR::epi.tests(tabela2x2, conf.level = 1-alfa)
sumario <- summary(out)
print(sumario, digits=4)
