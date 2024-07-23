# demo_vanBelle.R

B <- 1000
n <- 2
h <- NA
fim <- FALSE
tam_amostras <- c()
amplitudes <- c()
while (!fim)
{
  soma <- 0
  for (b in 1:B)
  {
    amostra <- rexp(n)
    res <- t.test(amostra,mu=0)
    soma <- soma+abs(res$conf.int[2]-res$conf.int[1])
  }
  amplitude.ic95 <- soma/B
  tam_amostras <- c(tam_amostras,n)
  amplitudes <- c(amplitudes,amplitude.ic95)
  # guarda a horizontal para n=12 
  if (n==12) {h <- amplitude.ic95}
  
  if (amplitude.ic95<0.7)
  {
    fim<-TRUE
  }
  n <- n+1
}
plot(tam_amostras, amplitudes,
     xlab="n", ylab="amplitude IC95%",
     type="l", lwd=2)
abline(v=12,lty=2)
abline(h=h,lty=2)
