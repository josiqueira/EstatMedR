ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
dt_treino <- data.frame(
  c(rep("Com",length(ComTreino)),
    rep("Sem",length(SemTreino))),
  c(ComTreino,SemTreino)
)
names(dt_treino) <- c("Treino","Simpatia")
dt_treino$Treino <- factor(dt_treino$Treino)
cat("\nCom treino:",ComTreino)
cat("\n\tmedia =",mean(ComTreino))
cat("\n\tmediana =",median(ComTreino))
cat("\n\tpseudomediana =",pm1 <- DescTools::HodgesLehmann(ComTreino))
cat("\nSem treino:",SemTreino)
cat("\n\tmedia =",mean(SemTreino))
cat("\n\tmediana =",median(SemTreino))
cat("\n\tpseudomediana =",pm2 <- DescTools::HodgesLehmann(SemTreino))
cat("\n")
cat("\nLocation shift = ", round(pm1-pm2,2), "\n", sep="")
print(wilcox.test(Simpatia~Treino,
                  data=dt_treino,
                  exact=FALSE,
                  correct=FALSE,
                  conf.int=TRUE,
                  conf.level=0.95))
plot(ecdf(ComTreino), 
     main="Empirical Cumulative Distribution Function",
     xlab="Escore de Simpatia", 
     ylab="Probabilidade",
     lwd=2, verticals = TRUE)
lines(ecdf(SemTreino), lwd=2, col="blue", verticals = TRUE)
abline(h=0.5,lty=3)
abline(v=3,lty=3)
abline(v=pm1,lty=3)
abline(v=pm2,lty=3,col="blue")
legend("topleft",
        c("Com treino", "Sem treino"),
        lty=c(1,1),
        lwd=c(1,2),
        col=c("black","blue"),
        bty="n")