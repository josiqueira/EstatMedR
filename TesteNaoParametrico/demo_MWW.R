ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
dt_treino <- data.frame(
  c(rep("Com",length(ComTreino)),
    rep("Sem",length(SemTreino))),
  c(ComTreino,SemTreino)
)
names(dt_treino) <- c("Treino","Simpatia")
dt_treino$Treino <- factor(dt_treino$Treino)

cat("\nCom treino:",ComTreino)
cat("\n\tmediana =",median(ComTreino))
cat("\nSem treino:",SemTreino)
cat("\n\tmediana =",median(SemTreino))
cat("\n")
dif <- median(ComTreino) - median(SemTreino)
cat("\nDiferenca das medianas amostrais (Com treino - Sem treino) = ",dif,sep="")
cat("\n")

cat("\nTeste U de Mann-Whitney Convencional:\n")
print(wilcox.test(Simpatia~Treino,
                  data=dt_treino,
                  exact=FALSE,
                  correct=FALSE,
                  conf.int=TRUE,
                  conf.level=0.95))
# Testing the equality of the distributions of a numeric response variable 
# in two or more independent groups against shift alternatives.
print(coin::wilcox_test(Simpatia~Treino,
                        data = dt_treino,
                        distribution=coin::approximate(nresample=1e6), 
                        conf.int = TRUE,
                        conf.level=0.95))

cat("\nTeste B de Brunner-Munzel:\n")
print(lawstat::brunner.munzel.test(SemTreino, ComTreino))

# Hollander & Wolfe, p. 110, results p. 111 and p. 126
cat("\nTeste U de Mann-Whitney Exato:\n")
dt_treino <- data.frame(
  treino = c(ComTreino,SemTreino),
  grupo = factor(rep(c("Com", "Sem"), 
                     c(length(ComTreino), length(SemTreino))))
)

