source ("eiras.show.MCSTAR.R")

Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
              dimnames = list(income = c("<15k", "15-25k",
                                         "25-40k", ">40k"),
                              satisfaction = c("VeryD", "LittleD",
                                               "ModerateS", "VeryS")))
print(Job)
nL <- nrow(Job) # ou dim(TC)[1]
nC <- ncol(Job) # ou dim(TC)[2]
df <- (nL-1)*(nC-1)
alfa <- 0.05

cat("\nTeste qui-quadrado de Pearson exato\n")
cat("(assumindo as variáveis como nominais):\n")
res <- chisq.test(Job,simulate.p.value=TRUE,B=1e6)
aux <- chisq.test(Job)
res$parameter <- aux$parameter
X2 <- res$statistic
N <- sum(res$observed)
print(res)

cat("\n------------------------------------------\n")
cat("Analise post hoc:")
cat("\n------------------------------------------\n")
cat("(assumindo as variáveis como nominais):\n")
STAR <- res$stdres # STandardized Adjusted Residual (STAR)
MCSTAR <- STAR/(sqrt((1-1/nL)*(1-1/nC))) # Moment-correct (MCSTAR)
show.MCSTAR(MCSTAR,alfa,df)
phi2 <- X2/N # phi ou w de Cohen
Dim <- min(nL,nC)-1 # dimensoes da TC: dim = max(phi^2)

# The Mantel-Haenszel chi-square statistic tests the alternative hypothesis
# that there is a linear association between the row variable and the
# column variable.
# Both variables must lie on an ordinal scale.

# 7.5 = (15-0)/2
# 20 = (25-15)/2
# 32.5 = (40-25)/2
# 60 = (160-40)/2
#
# > 20-7.5
# [1] 12.5
# > 32.5-20
# [1] 12.5
# > 60-32.5
# [1] 27.5
# > (160-40)/2
# [1] 60

mhtest <- DescTools::MHChisqTest(Job, srow=c(7.5,20,32.5,60))
print(mhtest)
# X-squared = 3.8075, df = 1, p-value = 0.05102
mhtest <- DescTools::MHChisqTest(Job)
print(mhtest)
# X-squared = 2.983, df = 1, p-value = 0.08414

# A diferenca está na suposicao que o valor máximo de income é 160.

# Approximative Linear-by-Linear Association Test
cointest <- coin::chisq_test(as.table(Job),
                             distribution = approximate(nresample = 1e6),
                             scores = list(income = 1:ncol(Job),
                                           satisfaction = 1:nrow(Job)))
print (cointest)

cat("\n------------------------------------------\n")
cat("Analise de significancia pratica:")
cat("\n------------------------------------------\n")
cat("(assumindo as variáveis como nominais):\n")

V <- sqrt(phi2/Dim) # V ou C de Cramer
cat("\nV de Cramer = ", round(V,4), "\n", sep="")
print(effectsize::interpret_cramers_v(V))
print(try(rcompanion::cramerV(Job,ci=TRUE,R=1e3)))

options(warn=0)

