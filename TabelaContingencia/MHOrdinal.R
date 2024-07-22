# Agresti (1990), apud Mehta, C. R. &
# Patel, N. R. (1996) SPSS Exact Tests 7 for  Windows. IL: SPSS, p. 72.
# Pap-Smear Classification by Two Pathologists
# O objetivo é analisar a concordância de diagnóstico entre 2 patologistas
# que  classificaram conforme a sereridade de uma determinada
# lesão uterina de 118 slides de diferentes mulheres.
# N=Negativo, HEA=Hiperplasia escamosa atipica, CIS=carcinoma in situ
# CE=Carcinoma escamoso, CI=Carcinoma invasivo
Tabela <- ("
 Patologistas N  HEA CIS CE CI
          N   22 2   2   0  0
          HEA 5  7   14  0  0
          CIS 0  2   36  0  0
          CE  0  1   14  7  0
          CI  0  0   3   0  3
")
print(TC <- as.matrix(read.table(textConnection(Tabela),
                                 header=TRUE, row.names=1)))
print(gplots::balloonplot(t(as.table(TC)),
													main ="", xlab ="", ylab="",
													label = FALSE,
													show.margins = FALSE))
cat("\nTeste de concordancia: delineamento intraparticipantes\n")
# Exact Marginal Homogeneity Test for Ordered Data
print(coin::mh_test(as.table(TC), distribution = "exact",
                    scores = list(response = 1:nrow(TC)))) # robusto
# data:  response (ordered) by
# conditions (Var1, Var2)
# stratified by block
# Z = 1.1523, p-value = 0.3073
# alternative hypothesis: two.sided
cat("Teste AC1 de Gwet para duas variáveis nominais\n")
cat("H0: AC1 = 0 vs H1: AC1 != 0\n",sep="")
print(irrCAC::gwet.ac1.table(TC))


