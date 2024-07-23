alfa <- 0.05
n <- 200
r <- 0.5 
ro <- 0.6
cat("\n")
cat("Teste de correlação:\n")
cat("\tH0: ro = ",ro," vs. H1: ro <> ",ro,"\n",sep="")
z <- sqrt((n-3)/2)*log(((1-ro)/(1+ro))/((1-r)/(1+r)))
p <- 2*pnorm(-abs(z))
cat("z = ",z,", p = ",p, sep="")
