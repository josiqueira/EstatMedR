alfa <- 0.05
n <- 200
r <- 0.5  
# H0: ro = 0 vs. H1: ro != 0
# https://en.wikipedia.org/wiki/Pearson_correlation_coefficient
cat("Teste bilateral:\n")
cat("\tH0: ro = 0 vs. H1: ro <> 0\n")
t <- r*sqrt((n-2)/(1-r^2))
p <- 2*pt(-abs(t), df=n-2)
cat("t(",n-2,") = ",t," , p = ",p,"\n",sep="")
correlacao <- DescTools::CorCI(r, n=n, conf.level = 1-alfa, 
                               alternative = "two.sided")
print(correlacao)
cat("\n")
cat("Teste unilateral a direita:\n")
cat("\tH0: ro = 0 vs. H1: ro > 0\n")
p <- 1-pt(t, df=n-2)
cat("t(",n-2,") = ",t," , p = ",p,"\n",sep="")
correlacao <- DescTools::CorCI(r, n=n, conf.level = 1-alfa, 
                               alternative = "greater")
print(correlacao)
cat("\n")
cat("Teste unilateral a esquerda:\n")
cat("\tH0: ro = 0 vs. H1: ro < 0\n")
p <- pt(t, df=n-2)
cat("t(",n-2,") = ",t," , p = ",p,"\n",sep="")
correlacao <- DescTools::CorCI(r, n=n, conf.level = 1-alfa, 
                               alternative = "less")
print(correlacao)

