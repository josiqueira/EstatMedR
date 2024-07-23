TH <- readRDS("Nutricao3.rds")
cat("Bootstrap One-Way ANOVA")
alfa <- 0.05
bootsamples <- 1e5
modelo_boot <- lmboot::ANOVA.boot(Sodium~Instructor, 
                                  B=bootsamples, 
                                  type="residual", 
                                  wild.dist="normal",  
                                  seed=123,
                                  data=TH, 
                                  keep.boot.resp=FALSE)
d <- density(modelo_boot$bootFStats)
plot(d, 
     main=paste("Independent one-way ANOVA (",bootsamples,
                " replicates)",sep=""), 
     xlab="F", ylab="Density", lwd=2)
Fc <- quantile(modelo_boot$bootFStats,probs = 1-alfa)
abline(v=Fc, lty=3)
Fobs <- qf(1-modelo_boot$`p-values`, modelo_boot$df[1],
           modelo_boot$df[2])
abline(v=Fobs, lty=4)
legend("topright",
       c("H0", 
         paste("F(",modelo_boot$df[1],",",modelo_boot$df[2],
               ",",1-alfa,") = ",round(Fc,3),sep=""),
         paste("F(",modelo_boot$df[1],",",modelo_boot$df[2],") = ",
               round(Fobs,3),"\n",
               "p = ",round(modelo_boot$`p-values`,4),sep="")
       ),
       lwd=c(2,1,1), lty=c(1,3,4),
       pt.bg="white",
       bty="n")
cat(paste("F(",modelo_boot$df[1],",",modelo_boot$df[2],") = ",
          round(Fobs,5),
          ", p = ",round(modelo_boot$`p-values`,5),"\n", sep=""))
cat(paste("(",bootsamples," bootstrap samples)\n", sep=""))

cat("Effect size analysis")
eta2 <- modelo_boot$df[1]*Fobs/(modelo_boot$df[1]*Fobs+modelo_boot$df[2])
print(eta2, digits = 6)
es <- effectsize::interpret_eta_squared(eta2)
names(es) <- c("Tamanho de efeito: estimativa pontual")
print(es)

