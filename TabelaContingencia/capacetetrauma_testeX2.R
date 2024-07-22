source("friendlycolor.R")

tabela <- as.table(matrix(c(17, 138, 130, 508), nrow = 2, byrow = TRUE))
colnames(tabela) <- c("Trauma +","Trauma -")
rownames(tabela) <- c("Capacete +","Capacete -")
alpha <- 0.05
chi2 <- chisq.test(tabela, correct=FALSE)
# vetor com valores de 0 a 8
chi2.valor <- seq(from=0, to=9, by=0.01)
graus.liberdade <- as.numeric(chi2$parameter)
# vetor com probabilidades correspondentes
p <- dchisq(chi2.valor, df=graus.liberdade)
# exibe o gráfico
plot(chi2.valor, p, type="l", ylim=c(0, 1.2),
     main="Teste X^2 (capacete e trauma), df=1",
     xlab="qui-quadrado", ylab="Densidade")
# vetor com os valores acima de qui critico 
chi2.critico <- qchisq(p=1-alpha, df=graus.liberdade)
chi2maioralfa <- chi2.valor[chi2.valor>chi2.critico] 
# probabilidades correspondentes
pmaioralfa <- dchisq(chi2maioralfa, df=graus.liberdade)
# hachura sob a curva
chi2maioralfa <- c(min(chi2maioralfa), chi2maioralfa, max(chi2maioralfa))
pmaioralfa <- c(0, pmaioralfa, 0)
polygon(chi2maioralfa, pmaioralfa, col=friendlycolor(10), border = NA)
# reforca a linha
lines(chi2maioralfa[2:(length(chi2maioralfa)-1)], 
      pmaioralfa[2:(length(pmaioralfa)-1)], 
      col=friendlycolor(10), lwd=2)
# linha pontilhada, valor critico
abline(v=chi2.critico, lty=2)

# ########## Area a direita de p ############
# vetor com os valores acima de qui calculado 
chi2maiorp <- chi2.valor[chi2.valor>chi2$statistic] 
# probabilidades correspondentes
pmaiorp <- dchisq(chi2maiorp, df=graus.liberdade)
# hachura sob a curva
chi2maiorp <- c(min(chi2maiorp), chi2maiorp, max(chi2maiorp))
pmaiorp <- c(0, pmaiorp, 0)
polygon(chi2maiorp, pmaiorp, col=friendlycolor(20), border = NA)
# reforca a linha
lines(chi2maiorp[2:(length(chi2maiorp)-1)], 
      pmaiorp[2:(length(pmaiorp)-1)], 
      col=friendlycolor(20), lwd=2)
# linha pontilhada, valor critico
abline(v=chi2$statistic, lty=3)

# legenda
legend("topright",
       c(paste("Qui^2 crítico=", round(chi2.critico,5), sep=""),
         paste("Qui^2 obs.=", round(chi2$statistic,5), sep=""), 
         paste("alfa=", alpha, sep=""), 
         paste("p=", round(chi2$p.value,5),sep="")), 
       col=c("black", "black", friendlycolor(10), friendlycolor(20)),
       lty=c(2, 3, 1, 1), 
       lwd=c(1, 1, 5, 5),
       box.lwd=0, bg="white",
       cex=0.8)  
