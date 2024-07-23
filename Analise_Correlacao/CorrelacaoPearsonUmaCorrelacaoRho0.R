alfa <- 0.05
n <- 200 
r <- 0.5
cat("\n")
cat("Teste de correlação:\n")
cat("\tH0: r = 0 vs. H1: r <> 0\n",sep="")
z <- sqrt((n-3)/2)*log(1/((1-r)/(1+r)))
p <- 2*pnorm(-abs(z))
cat("z = ",z,", p = ",p, sep="")

cat("\n\nAlternativamente:\n")
t <- r*sqrt((n-2)/(1-r^2))
p <- 2*pt(-abs(t), df=n-2)
cat("t(",n-2,") = ",t," , p = ",p,"\n",sep="")
