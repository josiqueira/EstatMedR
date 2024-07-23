v <- round(runif(10,1,10))
print(v)
n <- c()
m <- c()
s <- c()
o.v <- v
for (i in 1:10)
{
  n <- c(n,length(v))
  m <- c(m,mean(v))
  s <- c(s,sd(v))
  v <- c(v,o.v)
}
plot(n,m,xlab="n",ylab="media",type="b")
plot(n,s,xlab="n",ylab="desvio-padrao",type="b")
