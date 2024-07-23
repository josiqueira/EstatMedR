ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
alfa <- 0.05

# t test
B <- 1e5
Tstar <- rep(0,B)
difs <- c()
nC <- length(ComTreino)
nS <- length(SemTreino)
difindep <- mean(ComTreino)-mean(SemTreino)
epC <- sd(ComTreino)/sqrt(nC)
epS <- sd(SemTreino)/sqrt(nS)
epindep <- sqrt(epC^2 + epS^2) # erro-padrao da diferenca
for (i in 1:B)
{
  xC <- sample(ComTreino, nC, replace=TRUE)
  xS <- sample(SemTreino, nS, replace=TRUE)
  Tstar[i] <- (mean(xC) - mean(xS) - difindep) /
    sqrt(var(xC)/nC + var(xS)/nS)  
  difs <- c(difs,mean(xC)-mean(xS))
}
ICpv <- difindep + quantile(Tstar,c(alfa/2,0.5,1-alfa/2))*epindep
dens.T <- density(difindep+Tstar*epindep)
plot(dens.T, 
     main=paste0("One sample t test (boostraps = ",B,")"), 
     xlab="Diferenca das Medias Amostrais")
xm <- ICpv[2]
x1 <- ICpv[1]
x2 <- ICpv[3]
ym <- max(dens.T$y)/20
y1 <- ym+ym/2
y2 <- ym-ym/2
lines(c(x1,x1,x1,x2,x2,x2),c(y1,y2,ym,ym,y2,y1))
points(xm,ym,pch=21,col="black",bg="black")
abline(v=0,lty=2)
cat(paste0("\nOne sample t test (",B," reamostragens, bootstrapping pivotal)"))
cat(paste0("\nIntervalo de confiança 95% e mediana:\n"))
print(ICpv)

# ANOVA dataframe
dt_treino <- data.frame(
  c(rep("Com",length(ComTreino)),
    rep("Sem",length(SemTreino))),
  c(ComTreino,SemTreino)
)
names(dt_treino) <- c("Treino","Simpatia")
modelo_boot <- lmboot::ANOVA.boot(Simpatia~Treino, 
                                  B = B, 
                                  type = "residual", 
                                  wild.dist = "normal",  
                                  data = dt_treino, 
                                  keep.boot.resp = FALSE)
d <- density(modelo_boot$bootFStats)
dir.create("image",showWarnings=FALSE)
plot(d, 
     main=paste("Independent One-way ANOVA (",B," reamostragens)",sep=""), 
     xlab="F", ylab="Density", lwd=2)
Fc <- quantile(modelo_boot$bootFStats,probs = 0.95)
abline(v=Fc, lty=3)
Fobs <- qf(1-modelo_boot$`p-values`, modelo_boot$df[1], modelo_boot$df[2])
abline(v=Fobs, lty=4)
legend("topright",
       c("H0", 
         paste("F(",modelo_boot$df[1],",",modelo_boot$df[2],",",1-alfa,") = ",round(Fc,3),sep=""),
         paste("F(",modelo_boot$df[1],",",modelo_boot$df[2],") = ",round(Fobs,3),"\n",
               "p = ",round(modelo_boot$`p-values`,5),sep="")
       ),
       lwd=c(2,1,1), lty=c(1,3,4),
       pt.bg="white",
       bty="n")
cat(paste0("\nIndependent One-way ANOVA (",B," reamostragens, funcao lmboot::ANOVA.boot)\n"))
cat(paste("F(",modelo_boot$df[1],",",modelo_boot$df[2],") = ",
          round(Fobs,5),
          ", p = ",round(modelo_boot$`p-values`,5),"\n", sep=""))
