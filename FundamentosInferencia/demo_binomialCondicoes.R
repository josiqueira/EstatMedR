suppressMessages(library(fmsb, warn.conflicts = FALSE))
suppressMessages(library(DescTools, warn.conflicts = FALSE))

nivel <- c("R1", "R2", "R3", "Assistente")
errospuncao  <- c(24, 38, 8, 1)
tentativaspuncao <- c(61, 76, 55, 15)

cat("\nProporcao de erros de puncao:\n")
for (i in 1:length(nivel))
{
  cat("\t-",nivel[i],": ",errospuncao[i]," em ",
      tentativaspuncao[i]," = ",
      round((errospuncao[i]/tentativaspuncao[i])*100,2),"%",
      "\n",sep="")
}

IC95 <- DescTools::BinomCI(x=errospuncao, n=tentativaspuncao)
rownames(IC95) <- nivel
cat("\nIntervalos de confianca 95%:\n")
print(IC95)

omnibus <- prop.test(errospuncao, tentativaspuncao)
cat("\nTeste omnibus:")
print(omnibus)

posthoc <- fmsb::pairwise.fisher.test(errospuncao, tentativaspuncao)
colnames(posthoc$p.value) <- nivel[1:3]
rownames(posthoc$p.value) <- nivel[2:4]
cat("\nTeste post hoc:")
print(posthoc)

plot(NA,
     xlab="Proporção", ylab="",
     xlim=c(0,1.2),ylim=c(0,nrow(IC95)),
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
