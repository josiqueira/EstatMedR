max.n <- 1000

amostra.a <- rnorm(10,10,1)
amostra.b <- rnorm(10,6,3)
teste <- t.test(amostra.a, amostra.b)
plot(10,teste$statistic, xlim = c(0,max.n), ylim = c(0,max.n/20)
     , xlab = "Tamanho da Amostra", ylab = "Estatística t")

for (i in seq(20, max.n, by=10)) {
  amostra.a <- rnorm(i,10,1)
  amostra.b <- rnorm(i,6,3)
  teste <- t.test(amostra.a, amostra.b)
  y <- teste$statistic
  points(i,y)
}