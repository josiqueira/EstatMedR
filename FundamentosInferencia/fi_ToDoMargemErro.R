# IC95% por formula
alfa <- 0.05

# MCT Feminino
n.F <- sum(!is.na(Dados.F$MCT))
m.F <- mean(Dados.F$MCT, na.rm=TRUE)
sd.F <- sd(Dados.F$MCT, na.rm=TRUE)
se.F <- sd.F/sqrt(n.F)
cat("\n---- Feminino:\n")
cat("Media de MCT Feminino = ", 
    round(m.F,1), "\n", sep="")
cat("n Feminino = ", 
    round(n.F,1), "\n", sep="")
cat("Erro-padrao da media de MCT Feminino = ", 
    round(se.F,1), "\n", sep="")
me.F <- qt(p=1-alfa/2, df=n.F-1)*se.F
cat("Margem de erro da media de MCT Feminino = ", 
    round(me.F,1), "\n", sep="")
ic.F <- m.F+c(-1,1)*me.F
cat("IC95(media de MCT Feminino) = [", 
    round(ic.F,1), "]\n")

# MCT Masculino
n.M <- sum(!is.na(Dados.M$MCT))
m.M <- mean(Dados.M$MCT, na.rm=TRUE)
sd.M <- sd(Dados.M$MCT, na.rm=TRUE)
se.M <- sd.M/sqrt(n.M)
cat("\n\n---- Masculino:\n")
cat("Media de MCT Masculino = ", 
    round(m.M,1), "\n", sep="")
cat("n Masculino = ", 
    round(n.M,1), "\n", sep="")
cat("Erro-padrao da media de MCT Masculino = ", 
    round(se.M,1), "\n", sep="")
me.M <- qt(p=1-alfa/2, df=n.M-1)*se.M
cat("Margem de erro da media de MCT Masculino = ", 
    round(me.M,1), "\n", sep="")
ic.M <- m.M+c(-1,1)*me.M
cat("IC95(media de MCT Masculino) = [", 
    round(ic.M,1), "]\n")
