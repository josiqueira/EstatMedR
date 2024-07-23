# demo_PseudoMediana.R

v <- c(8,8,11,12,20)

cat("\nValores:",v," (total de",length(v),"valores)\n")
m <- c()
par <- 0
for (i1 in 1:length(v))
{
  for (i2 in i1:length(v))
  {
    par <- par+1
    cat("\npar ",par,": ",v[i1]," e ",v[i2],
        ", média = ",mean(c(v[i1],v[i2])),sep="")
    m <- c(m,mean(c(v[i1],v[i2])))
  }
}
cat("\n\nPseudomediana = ",median(m),
", obtida de ",par," pares de valores.\n",sep="")
