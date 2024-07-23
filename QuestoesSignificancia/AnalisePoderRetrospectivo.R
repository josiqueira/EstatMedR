suppressMessages(library(MBESS, warn.conflicts = FALSE))
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
cat(paste("\nn total =", k*n, "\tFcrt =", round(Fcrt,2),"\tFobs =", round(Fobs,2),"\n"))
poder <- 1-pf(Fcrt,dfn, dfd, ncp)
cat(paste("\tPoder Retrospectivo =", round(poder,3),"\n"))
poder.ll <- 1-pf(Fcrt,dfn, dfd, ncp.ll)
cat(paste("\tPoder.ll.95 =", round(poder.ll,3),"\n"))
poder.ul <- 1-pf(Fcrt,dfn, dfd, ncp.ul)
cat(paste("\tPoder.ul.95 =", round(poder.ul,3),"\n"))
# sink()
