SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
cat("Com treino:",ComTreino,"\n")
cat("Sem treino:",SemTreino,"\n")
dif <- median(ComTreino) - median(SemTreino)
cat("Diferenca das medianas amostrais (Com treino - Sem treino) = ",
    dif,"\n",sep="")

# print(brunnermunzel::brunnermunzel.test(value ~ group, data = dat))
# print(brunnermunzel::brunnermunzel.test(SemTreino, ComTreino, perm = TRUE))
print(brunnermunzel::brunnermunzel.permutation.test(SemTreino, 
                                                    ComTreino, 
                                                    force=TRUE))