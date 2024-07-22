library(irrCAC)
source("eiras.matrix2dataframe.R")

# sink("TC2x2_Gwet.txt")
# pdf("TC2x2_Gwet.pdf")

# GWET, KL (2008) Computing inter-rater reliability and its variance
# in the presence of high agreement. British Journal of Mathematical and
# Statistical Psychology 61: 29-48.
Tabela <- ("
  BellxKK P   N
  P       184 54
  N       14  63
")
cat(Tabela)
# Foram analisadas 315 amostras usando os métodos Bell e
# Kato-Katz para detecção ovos de Schistosoma mansoni nas fezes.
# Sleigh A, Hoff R, Mott K, Barreto M, de Paiva TM, Pedrosa Jde S,
# Sherlock I. (1982)
# Comparison of filtration staining (Bell) and thick smear (Kato)
# for the detection of quantitation of Schistosoma mansoni eggs
# in faeces. Transactions of the Royal Society of
# Tropical Medicine and Hygiene 76(3):403-6.
# doi: 10.1016/0035-9203(82)90201-2. PMID: 7112662.
TC <- as.matrix(read.table(textConnection(Tabela),
													 header=TRUE, row.names=1))
print(gplots::balloonplot(t(as.table(TC)), main ="", xlab ="", ylab="",
													label = FALSE, show.margins = FALSE))
# AC1 de Gwet (2008)
cat("\nAC1 de Gwet",sep="")
AC1 <- irrCAC::gwet.ac1.table(ratings=TC)
AC1.f <- AC1$coeff.val
AC1.ic <- AC1$coeff.ci
AC1.p <- AC1$coeff.pval
cat("\n\tAC1 = ",AC1.f, ", IC95 ",AC1.ic,", p=",AC1.p,"\n",sep="")
# dev.off()
# sink()
