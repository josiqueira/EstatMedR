library(MBESS)
# sink("Analise de poder retrospectivo ou a posteriori.txt")
cat("ANOVA unifatorial independente balanceada")
k <- 2 # numero de condicoes independentes
n <- 30 # tamanho da amostra em cada condicao independente
alfa <- 0.05
dfn <- k - 1
dfd <- k*(n - 1)
Fcrt <- qf(1-alfa, dfn, dfd)
Fobs <- Fcrt*0.99 # Fobs um pouco menor que Fcrit: nao rejeita H0 por um triz
eta2 <- dfn*Fobs/(dfn*Fobs+dfd)
eta2lims <- MBESS::ci.pvaf(Fobs, dfn, dfd, k*n, 1-alfa)
f2 <- eta2/(1-eta2)
f2.ll <- eta2lims$Lower.Limit.Proportion.of.Variance.Accounted.for/
  (1-eta2lims$Lower.Limit.Proportion.of.Variance.Accounted.for)
f2.ul <- eta2lims$Upper.Limit.Proportion.of.Variance.Accounted.for/
  (1-eta2lims$Upper.Limit.Proportion.of.Variance.Accounted.for)
ncp <- dfd*f2 # ou dfn*Fobs
ncp.ll <- dfd*f2.ll 
ncp.ul <- dfd*f2.ul 
cat("\n\tn total = ", k*n,
          "\n\tk (numero de grupos) = ", k, 
          "\n\tF critico (alfa = ", alfa, ") = ", round(Fcrt,2),
          "\n\tF = ", round(Fobs,2),"\n", sep="")
poder <- 1-pf(Fcrt,dfn, dfd, ncp)
cat(paste("\tEstimativa pontual do poder retrospectivo =", round(poder,3),"\n"))
poder.ll <- 1-pf(Fcrt,dfn, dfd, ncp.ll)
poder.ul <- 1-pf(Fcrt,dfn, dfd, ncp.ul)
cat("\tIC95 do poder retrospectivo = [", round(poder.ll,3),", ",
          round(poder.ul,3),"]\n", sep="")
# sink()
