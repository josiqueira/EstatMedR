source("eiras.matrix2dataframe.R")
# Siegel & Castellan (1988), p. 77
Tabela <- ("
 TVDebate Carter Reagan
  Carter  28     13
  Reagan  7      27
")
cat(Tabela)
TC <- as.matrix(read.table(textConnection(Tabela),
  												 header=TRUE, row.names=1))
print(coin::mh_test(as.table(TC), distribution = "exact")) # robusto
print(mcnemar.test(TC,correct=FALSE)) # classico
print(exact2x2::mcnemar.exact(TC))
print(gplots::balloonplot(t(as.table(TC)), main ="",
													xlab ="", ylab="",
                          label = FALSE,
													show.margins = FALSE))
