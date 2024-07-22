# Teste qui-quadrado de Mantel-Haenszel.R

# desabilita warnings
options(warn=-1)

# Tabela de contingencia 2x2 segmentada
# Teste robusto da razao de chances (OR) de Mantel-Haenszel

# Relacao entre tabagismo e sobrevivência em 20 anos (1974-1994)
# em 1.134 mulheres adultas do Reino Unido
# Delineamento: coorte
# Fonte:  APPLETON, D. R. et al. (1996) Ignoring a covariate:
# An example of Simpson's paradox. The American Statistician,
# 50(4): 340-1.

# A aspa inicial TEM que comecar na primeira coluna da linha
# e os espacamentos distintos dos dessa tabela podem causar
# problemas de na geracao da tabela horizontalizada (ftable).
suppressMessages(library(vcd))
library(rcompanion, warn.conflicts = FALSE)
suppressMessages(library(coin))
library(readxl)
suppressMessages(library(tidyverse))
TCtmp <- readxl::read_excel("fumo_e_faixaetaria.xlsx")
TCtmp <- tidyr::gather(TCtmp, var, Contagem, -Idade)
TCtmp <- tidyr::separate(TCtmp, var, c('Exposicao','Desfecho'))
TCS <- TCtmp %>% xtabs(Contagem ~ factor(Exposicao, levels=c("Tabagista","NaoTabagista")) +
                         factor(Desfecho, levels=c("Morta", "Viva")) +
                         Idade, .)
print(TCS)

# Mantel-Haenszel test of the null that two nominal variables
# are conditionally independent in each stratum,
# assuming that there is no three-way interaction.
# Woolf test for homogeneity of ORs across strata on 2 x 2 x k tables:
# If significant, M-H test is not appropriate.
cat("\nTabela global horizontalizada:\n")
prmatrix(ftable(TCS)) # Display a flattened table (tabela horizontalizada)

cat("\nHomogeneidade dos OR:\n")
print(vcd::woolf_test(TCS))

# Teste para tabela 2x2xK
cat("\nQui-quadrado de Mantel-Haenszel:\n")
print(mh <- mantelhaen.test(TCS, exact=TRUE)) # Teste exato de OR de Mantel-Haenszel

# Teste para tabela LxCxK
cat("\nTeste de Cochran-Mantel-Haenszel (coin::cmh_test):\n")
print(coin::cmh_test(TCS, distribution = coin::approximate(nresample = 1e5)))

cat("\nAnalise estratificada por idade:\n")
print(rcompanion::groupwiseCMH(TCS,
                               group   = 3,
                               fisher  = TRUE,
                               gtest   = FALSE,
                               chisq   = FALSE,
                               method  = "bonferroni",
                               correct = "none",
                               digits  = 3))
cat("\n")
print(ors <- vcd::oddsratio(TCS, log=FALSE)) # Show OR for each 2x2
cat("\n")
lnors <- vcd::oddsratio(TCS, log=TRUE) # Show ln(OR) for each 2x2
print(lnors)
print(summary(lnors))
print(confint(lnors))
plot(lnors, main="Intervalos de confiança por faixa etária",
		 xlab = "Faixa Etaria", ylab = "ln(OR)")

# reabilita warnings
options(warn=0)
