# TabelaContingencia2x2_TabagismoMorte.R
# paradoxo de Simpson

source("eiras.show.MCSTAR.R")

# desabilita warnings
options(warn=-1)

TCtmp <- readxl::read_excel("fumo_e_faixaetaria_total.xlsx")
TC <- data.matrix(TCtmp)
rownames(TC) <- as.character(unlist(TCtmp[1:2,1]))
TC <- TC[,-1]
remove(TCtmp)
cat("\n------------------------------------------\n")
cat("Dados:")
cat("\n------------------------------------------\n")
print(TC)

cat("\n------------------------------------------\n")
cat("Analise de significancia estatistica:")
cat("\n------------------------------------------\n")
alfa <- 0.05
B <- 1e5

cat("\nTeste qui-quadrado de Pearson exato:\n")
print(res <- chisq.test(TC,simulate.p.value=TRUE,B=B))
cat("X^2 critico de 95% = ",qchisq(p=1-alfa,df=1), "\n", sep="")
N <- sum(res$observed)
nL <- nrow(TC) # ou dim(TC)[1]
nC <- ncol(TC) # ou dim(TC)[2]
df <- (nL-1)*(nC-1)
X2 <- res$statistic # estatistica de teste qui-quadrado
cat("Graus de liberdade (nao fornecidos por bootstrapping): ", df, "\n", sep="")
cat("Heuristica de significancia (rej. H0 se X^2/gl > 2): ", X2/df, "\n", sep="")

cat("\n------------------------------------------\n")
cat("Analise post hoc:")
cat("\n------------------------------------------\n")
STAR <- res$stdres # STandardized Adjusted Residual (STAR)
MCSTAR <- STAR/(sqrt((1-1/nL)*(1-1/nC))) # Moment-correct (MCSTAR)
show.MCSTAR(MCSTAR=MCSTAR,alpha=alfa,df=df)

cat("\n------------------------------------------\n")
cat("Analise de significancia pratica: V de Cramer")
cat("\n------------------------------------------\n")
phi2 <- X2/N # phi ou w de Cohen
Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)
V <- sqrt(phi2/Dim) # V ou C de Cramer
cat("\nV de Cramer = ", round(V,4), "\n", sep="")
print(effectsize::interpret_cramers_v(V))
print(try(rcompanion::cramerV(TC,ci=TRUE,R=1e3)))

cat("\nTeste de razao de chances (OR):\n")
resft <- exact2x2::fisher.exact(TC,conf.level=1-alfa) # Teste de OR
print(resft)

# reabilita warnings
options(warn=0)

