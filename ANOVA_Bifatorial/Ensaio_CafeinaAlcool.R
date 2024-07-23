#Ensaio: Cafeina e Alcool

Nivel_A <- 1:5
Erros_incA <- 1+0.4*Nivel_A
Erros_SemA <- rep(10,5)
Erros_ComA <- Erros_SemA
Erros_ComA[1] <- Erros_ComA[1]+Erros_incA[1]*runif(1)  
for (n in 2:5)
{
  Erros_ComA[n] <- Erros_ComA[n-1]+Erros_incA[n]*runif(1)  
}

# Agua eh um antidoto putativo
# hidrata, contrabalancando o Alcool
# reduz o efeito em cerca de 20 a 30%
# não importa o nivel alcoolico (sem interacao)
Interacao <- rep(0,5)
for(r in 1:100)
{
  plot(Nivel_A,Erros_SemA,ylim=c(0,30),type="b",
       main="Agua reduz os erros em 20 a 30% sem interacao\n(nao importa o nivel alcoolico)",
       xlab="Nivel Alcoolico")
  lines(Nivel_A,Erros_ComA, col="red")
  points(Nivel_A,Erros_ComA, col="red")
  Erros_ComAComC <- Erros_ComA
  for (n in 1:5)
  {
    Erros_ComAComC[n] <- Erros_ComAComC[n] - Erros_ComAComC[n]*runif(5,0.2+Interacao[n],0.3+Interacao[n])
  }
  lines(Nivel_A,Erros_ComAComC, col="blue")
  points(Nivel_A,Erros_ComAComC, col="blue")
  Sys.sleep(0.2)  
}

# Barbiturico eh um agonista putativo
# reforca o efeito do Alcool
# aumenta o efeito em cerca de 20 a 30%
# (mais eficaz quando o nivel alcoolico é mais alto)
Interacao <- seq(0.05,0.7,length.out = 5)
for(r in 1:100)
{
  plot(Nivel_A,Erros_SemA,ylim=c(0,30),type="b",
       main="Barbiturico aumenta os erros em 20 a 30%\n(pior quanto maior o nivel alcoolico)",
       xlab="Nivel Alcoolico")
  lines(Nivel_A,Erros_ComA, col="red")
  points(Nivel_A,Erros_ComA, col="red")
  Erros_ComAComC <- Erros_ComA
  for (n in 1:5)
  {
    Erros_ComAComC[n] <- Erros_ComAComC[n] + Erros_ComAComC[n]*runif(5,0.2+Interacao[n],0.3+Interacao[n])
  }
  lines(Nivel_A,Erros_ComAComC, col="brown")
  points(Nivel_A,Erros_ComAComC, col="brown")
  Sys.sleep(0.2)  
}

# Super-X eh um antidoto putativo
# aumenta o alerta, contrabalancando o Alcool
# reduz o efeito em cerca de 20 a 30%
# (mais eficaz quando o nivel alcoolico é mais alto)
Interacao <- seq(0.05,0.7,length.out = 5)
for(r in 1:100)
{
  plot(Nivel_A,Erros_SemA,ylim=c(0,30),type="b",
       main="Super-X reduz os erros em 20 a 30% com interacao\n(mais eficaz com maior nivel alcoolico)",
       xlab="Nivel Alcoolico")
  lines(Nivel_A,Erros_ComA, col="red")
  points(Nivel_A,Erros_ComA, col="red")
  Erros_ComAComC <- Erros_ComA
  for (n in 1:5)
  {
    Erros_ComAComC[n] <- Erros_ComAComC[n] - Erros_ComAComC[n]*runif(5,0.2+Interacao[n],0.3+Interacao[n])
  }
  lines(Nivel_A,Erros_ComAComC, col="green")
  points(Nivel_A,Erros_ComAComC, col="green")
  Sys.sleep(0.2)  
}

# Agua eh um antidoto putativo
# hidrata, contrabalancando o Alcool
# reduz o efeito em cerca de 20 a 30%
# (mais eficaz quando o nivel alcoolico é mais baixo)
Interacao <- seq(0.7,0.05,length.out = 5)
Erros_ComAComC2 <- Erros_ComA
for (n in 1:5)
{
  Erros_ComAComC2[n] <- Erros_ComAComC2[n] - Erros_ComAComC2[n]*runif(5,0.2+Interacao[n],0.3+Interacao[n])
}
lines(Nivel_A,Erros_ComAComC2, col="darkgreen")
points(Nivel_A,Erros_ComAComC2, col="darkgreen")
