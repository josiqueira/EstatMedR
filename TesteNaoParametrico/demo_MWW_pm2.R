library(lawstat)
source("eiras.pseudomediana.R")

ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)

# cat("\nCom treino:",ComTreino)
# comb1 <- combn(ComTreino,2)
# cat("\n\tpares:",ncol(comb1)," combinações possíveis",sep="")
# m1 <- rep(0,ncol(comb1))
# for (i in 1:ncol(comb1))
# {
#   m1[i] <- median(comb1[,i])
# }
# cat("\n\tpseudomediana =",median(m1))
# cat("\nSem treino:",SemTreino)
# comb2 <- combn(SemTreino,2)
# cat("\n\tpares:",ncol(comb2)," combinações possíveis",sep="")
# m2 <- rep(0,ncol(comb2))
# for (i in 1:ncol(comb2))
# {
#   m2[i] <- mean(comb2[,i])
# }
# cat("\n\tpseudomediana =",median(m2))

cat("\nCom treino:",ComTreino)
pm <- pseudomediana(ComTreino)[["statistics"]]
cat("\n\tpares: ",pm[2]," combinações possíveis",sep="")
cat("\n\tpseudomediana =",pm[1])
cat("\nSem treino:",SemTreino)
pm <- pseudomediana(SemTreino)[["statistics"]]
cat("\n\tpares: ",pm[2]," combinações possíveis",sep="")
cat("\n\tpseudomediana =",pm[1])


