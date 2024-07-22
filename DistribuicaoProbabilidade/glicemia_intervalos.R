library(lawstat)
library(diptest)

alfa <- 0.05
glicemia <- c(5.5, 5.2, 5.2, 5.8, 5.6, 4.6, 
              5.6, 5.9, 4.7, 5.0, 5.7, 5.2)
m <- mean(glicemia)
s <- sd(glicemia)
print(ressw <- shapiro.test(glicemia)) # teste de unimormalidade 
print(ressim <- lawstat::symmetry.test(sort(glicemia),option="MGG",boot=T,B=1e4))
print(resdip <- diptest::dip.test(sort(glicemia),simulate.p.value=T,B=1e4)) # Hartigans' Dip Test for Unimodality
cat("Distribuicao da glicemia:\n")
if (ressw$p.value>alfa) 
{cat("\tNormal (Shapiro-Wilk test)\n")} else
{cat("\tNao-normal (Shapiro-Wilk test)\n")}
if (ressim$p.value>alfa) 
{cat("\tSimetrica (bootstrap Miao et al symmetry test)\n")} else
{cat("\tAssimetrica (bootstrap Miao et al symmetry test)\n")}
if (resdip$p.value>alfa) 
{cat("\tUnimodal (bootstrap Hartigans dip test for unimodality)\n\n")} else
{cat("\tNao-unimodal (bootstrap Hartigans dip test for unimodality)\n\n")}
cat("Intervalo de predicao de 95 simetrico centrado na media\n")

iqr <- IQR(glicemia,na.rm=TRUE)
notfound <- TRUE
if (ressw$p.value>alfa) {
  k <- qt(1-alfa/2, df=length(glicemia)-1)
  ipnome <- "Normal"
  notfound <- FALSE
}
if (notfound && ressw$p.value<=alfa && resdip$p.value>alfa && ressim$p.value>alfa) {
  k <- (2/3)*sqrt(1/alfa)
  ipnome <- "Gauss-Camp-Meidell unimodal simetrica nao-normal"
  notfound <- FALSE
} 
if (notfound && ressw$p.value<=alfa && resdip$p.value>alfa && ressim$p.value<=alfa) {
  a <- abs(m-moda)/iqr
  k <- a + (2/3)*sqrt((1-a^2)/alfa)
  ipnome <- "Gauss-Camp-Meidell unimodal assimetrica nao-normal"
  notfound <- FALSE
}
if (notfound) {
  k <- sqrt(1/alfa)
  ipnome <- "Chebychev para qualquer distribuicao"
} 
lb95pi <- m - k*s 
ub95pi <- m + k*s
cat("Intervalo de Predicao (IP)\n")
cat("  IP95 de ",ipnome, " = [",lb95pi,",",ub95pi,"]\n", sep="")
