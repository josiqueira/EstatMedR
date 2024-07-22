# TCrXc_StatusOcupacional.R

library(datasets)
library(DescTools)

source ("eiras.cramerV.R")
source ("eiras.show.MCSTAR.R")

# desabilita warnings
options(warn=-1)

cat("\n------------------------------------------\n")
cat("Dados:")
cat("\n------------------------------------------\n")
# Duncan, O.D. (1979), How Destination Depends on Origin in the
# Occupational Mobility Table. American Journal of Sociology 84, 793-803.
tcrxc <- datasets::occupationalStatus
# para recolocar as categorias originais
categories <- c("I","II","III","IV","Va","Vb","VI","VII")
rownames(tcrxc) <- categories
colnames(tcrxc) <- categories
prmatrix(tcrxc)

cat("\n------------------------------------------\n")
cat("Analise de significancia estatistica:")
cat("\n------------------------------------------\n")
alfa <- 0.05

cat("\nTeste qui-quadrado de Pearson exato\n")
cat("(assumindo as variáveis como nominais):\n")
res <- chisq.test(tcrxc,simulate.p.value=TRUE,B=1e5)
aux <- chisq.test(tcrxc)
res$parameter <- aux$parameter
print(res)

N <- sum(res$observed)
nL <- nrow(tcrxc) # ou dim(TC)[1]
nC <- ncol(tcrxc) # ou dim(TC)[2]
df <- (nL-1)*(nC-1)
cat("X^2 critico de ",round((1-alfa)*100,1),"% = ",qchisq(p=1-alfa,df=df), "\n", sep="")
X2 <- res$statistic # estatistica de teste qui-quadrado
cat("Graus de liberdade (nao fornecidos por bootstrapping): ", df, "\n", sep="")
cat("Heuristica de significancia (rej. H0 se X^2/gl > 2): ", X2/df, "\n", sep="")

cat("\nTestes Gama de Goodman-Kruskal e OR generalizado\n")
cat("(assumindo as variáveis como ordinais):\n")

# Goodman, L. A. (1979) Simple Models for the Analysis of Association
# in Cross-Classifications having Ordered Categories.
# J. Am. Stat. Assoc., 74 (367), 537–552.
# gama <- DescTools::GoodmanKruskalGamma(tcrxc)
gama <- vcdExtra::GKgamma(tcrxc,level=1-alfa)
cat("\nGama de Goodman-Kruskal = ", gama$gamma,
    ", IC",round((1-alfa)*100,1)," [",gama$CI[1],",",gama$CI[2],"]\n", sep="")

# EDWARDES, MD & BALTZAN, M (2000) The generalization of the odds ratio,
# rik ratio and risk difference to rxk tables.
# Statistics in Medicine 19:1901-14.
ORg <- (1+gama$gamma)/(1-gama$gamma)
ORg.LI <- (1+gama$CI[1])/(1-gama$CI[1])
ORg.LS <- (1+gama$CI[2])/(1-gama$CI[2])
cat("OR bruto generalizado = (1 + ",gama$gamma,")/(1 - ",gama$gamma,") = ",ORg,sep="")
cat(", IC",round((1-alfa)*100,1)," [",ORg.LI, ",", ORg.LS,"]\n",sep="")

cat("\n------------------------------------------\n")
cat("Analise post hoc:")
cat("\n------------------------------------------\n")
cat("(assumindo as variáveis como nominais):\n")

STAR <- res$stdres # STandardized Adjusted Residual (STAR)
MCSTAR <- STAR/(sqrt((1-1/nL)*(1-1/nC))) # Moment-correct (MCSTAR)
show.MCSTAR(MCSTAR,alfa,df)
phi2 <- X2/N # phi ou w de Cohen
Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)

cat("\n------------------------------------------\n")
cat("Analise de significancia pratica:")
cat("\n------------------------------------------\n")
cat("(assumindo as variáveis como nominais):\n")

V <- sqrt(phi2/Dim) # V ou C de Cramer
cat("\nV de Cramer = ", round(V,4), "\n", sep="")
print(effectsize::interpret_cramers_v(V))
print(try(rcompanion::cramerV(tcrxc,ci=TRUE,R=1e3)))

# reabilita warnings
options(warn=0)

