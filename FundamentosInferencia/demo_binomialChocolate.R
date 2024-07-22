suppressMessages(library(RVAideMemoire, warn.conflicts = FALSE))

alpha <- 0.05

chocolate <- c("Best Cocoa", "Dream Brown", "Wonka")
preferido <- c(23, 12, 44)
# reordenar por ordem de preferencia
df.tmp <- data.frame(chocolate,preferido)
df.tmp <- df.tmp[order(df.tmp$preferido),]
chocolate <- df.tmp$chocolate
preferido <- df.tmp$preferido

IC95 <- DescTools::MultinomCI(preferido, conf.level=0.95)
rownames(IC95) <- chocolate
print(IC95)

plot(NA,
     xlab="Proporção", ylab="",
     xlim=c(0,1),ylim=c(0,nrow(IC95)),
     axes=FALSE)
axis(1, at=seq(0,1,0.1))
for (r in 1:nrow(IC95))
{
  y <- nrow(IC95)-r+1
  lines(c(IC95[r,2],IC95[r,3]),rep(y,2))
  points(c(IC95[r,2],IC95[r,3]),rep(y,2),pch="I")
  points(IC95[r,1],y,pch=21,col="black",bg="black")
  text(IC95[r,3], y, rownames(IC95)[r], pos=4)
}

# teste
chi2 <- chisq.test(preferido, p=rep(1/3,3), simulate.p.value=TRUE, B=1e5)
cat("\n----------------------------\n")
cat ("Teste omnibus:\n")
print (chi2)
cat("\tInfere-se, para a populacao, que a preferencia ",sep="")
if (chi2$p.value < alpha)
{
  cat("por pelo menos um dos chocolates difere de algum outro.")
} else
{
  cat("por todos os chocolates e igual´.\n")
}
cat("\n")

# aplica o teste par a par
if (chi2$p.value < alpha)
{
  cat("\n----------------------------\n")
  cat ("Analise par a par:\n")
  pares <- RVAideMemoire::multinomial.multcomp(preferido, 
                                               p.method="holm")
  colnames(pares$p.value) <- chocolate[1:2]
  rownames(pares$p.value) <- chocolate[2:3]
  print (pares)
  cat("\n")
  cat("\tInfere-se, para a populacao, entre:\n",sep="")
  for (r in 1:nrow(pares$p.value))
  {
    for (c in 1:ncol(pares$p.value))
    {
      if (!is.na(pares$p.value[r,c]))
      {
        cat("\t\t- ",rownames(pares$p.value)[r]," e ",colnames(pares$p.value)[c],": ",sep="")
        if (pares$p.value[r,c] >= alpha)
        {
          cat ("nao ")
        }
        cat ("ha diferenca de preferencia entre os chocolates\n")
      }
    }
  }
}
