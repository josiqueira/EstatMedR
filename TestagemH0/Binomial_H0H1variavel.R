source("eiras.friendlycolor.R")

# defaults

alfa <- 0.01
jogadas <- 15
pH0 <- 0.5

c_poder <- c()
for (dif in 1:jogadas)
{
  pH1 <- dif/jogadas
  sucesso <- 0:jogadas
  H0 <- dbinom(sucesso,jogadas,pH0)
  H1 <- dbinom(sucesso,jogadas,pH1)
  
  # cutoff 5%
  cutoff <- jogadas+1
  while (sum(H0[cutoff:(jogadas+1)]) < alfa)
  {
    cutoff <- cutoff-1
  }
  # beta
  beta <- sum(dbinom(0:(cutoff-1),jogadas,pH1))
  c_poder <- c(c_poder,(1-beta)*100)
}
plot(NA,xlim=c(0,jogadas),ylim=c(0,100),xlab="Jogadas",ylab="Poder (%)", axes=FALSE)
axis(1, at=1:jogadas, cex=0.7)
axis(2)
for (dif in 1:jogadas)
{
  lines(c(dif-0.45,dif+0.45), rep(c_poder[dif],2), lwd=3)
}
abline(h=80,lty=2)
