source("eiras.create.population.R")

s <- 27863
set.seed(s)
col1 <- "#000000"
col2 <- eiras::FriendlyColor(8)
col1t <- paste0(col1,"30")
col2t <- paste0(col2,"30")
numamostras <- 15000
n1 <- 6
n2 <- 8
alpha <- 0.05

pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(135, 160, 210),
                          sd=c(16,17,18))
pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(111, 186, 236),
                          sd=c(19,20,23))

# amostras
rm(.Random.seed, envir=globalenv()) # reset seed

c_tstud <- c()
c_twelch <- c()
c_utrad <- c()
a <- b <- c <- d <- c(0,0,0)
for (amostra in 1:numamostras)
{
  amostra1 <- sample(pop1,size=n1)
  amostra2 <- sample(pop2,size=n2)
  
  # cat("\nTeste t de Student:\n")
  t <- t.test(amostra1,amostra2,var.equal=TRUE)
  p.tstud <- t$p.value
  c_tstud <- c(c_tstud, p.tstud)
  
  # cat("\nTeste t de Welch/Satterthwite:\n")
  t <- t.test(amostra1,amostra2)
  p.twelch <- t$p.value
  c_twelch <- c(c_twelch, p.twelch)
  
  # cat("\nTeste U de Mann-Whitney Convencional:\n")
  u <- wilcox.test(amostra1,amostra2, exact = FALSE)
  p.utrad <- u$p.value 
  c_utrad <- c(c_utrad, p.utrad)
  
  # t Student vs U MWW
  if (p.tstud<alpha & p.utrad<alpha) {a[1] <- a[1]+1}
  if (p.tstud<alpha & p.utrad>=alpha) {b[1] <- b[1]+1}
  if (p.tstud>=alpha & p.utrad<alpha) {c[1] <- c[1]+1}
  if (p.tstud>=alpha & p.utrad>=alpha) {d[1] <- d[1]+1}
  # t Welch vs U MWW
  if (p.twelch<alpha & p.utrad<alpha) {a[2] <- a[2]+1}
  if (p.twelch<alpha & p.utrad>=alpha) {b[2] <- b[2]+1}
  if (p.twelch>=alpha & p.utrad<alpha) {c[2] <- c[2]+1}
  if (p.twelch>=alpha & p.utrad>=alpha) {d[2] <- d[2]+1}
  # t Student vs t Welch
  if (p.tstud<alpha & p.twelch<alpha) {a[3] <- a[3]+1}
  if (p.tstud<alpha & p.twelch>=alpha) {b[3] <- b[3]+1}
  if (p.tstud>=alpha & p.twelch<alpha) {c[3] <- c[3]+1}
  if (p.tstud>=alpha & p.twelch>=alpha) {d[3] <- d[3]+1}
}


cat("\nSimulação com total de ",numamostras," amostragens.\n")
pares <- c("U MWW","t Student",
           "U MWW","t Welch",
           "t Welch","t Student"
           )
i2 <- 1
for (i in 1:3)
{
  cat("\nTabela de contingencia (concordancia entre os testes):\n")
  m <- matrix(c(a[i],b[i],c[i],d[i]),ncol=2,nrow=2,byrow=TRUE)
  colnames(m) <- c(paste0(pares[i2],":H1"),paste0(pares[i2],":H0"))
  rownames(m) <- c(paste0(pares[i2+1],":H1"),paste0(pares[i2+1],":H0"))
  print (m)

  cat(paste0("\nTeste da concordância entre ",
             pares[i2]," e ",pares[i2+1],":\n"))
  cat("\n\tH0: G =  0")
  cat("\n\tH1: G <> 0")
  G <- ((a[i]+d[i])-(b[i]+c[i]))/numamostras
  z <- (a[i]+d[i] - numamostras/2)/sqrt(numamostras/4)
  p <- 2*(1-pnorm(abs(z)))
  cat("\n")
  cat("\n\tG = ",G,sep="")
  cat("\n\tz = ",z,", p = ",p,sep="")
  cat("\n\nConclusão:")
  if(p<0.05)
  {
    cat(" há concordância")
  } else
  {
    cat(" não há associação")
  }
  cat(paste0(" entre os testes ",pares[i2]," e ",pares[i2+1],"."))
  cat("\n")

  i2 <- i2+2  
} # for i

r_utrad <- sum(c_utrad<alpha,na.rm=TRUE)/numamostras
r_tstud <- sum(c_tstud<alpha,na.rm=TRUE)/numamostras
r_twelch <- sum(c_twelch<alpha,na.rm=TRUE)/numamostras
cat("\n")
cat("\nProporcao de Rejeicoes corretas:")
cat("\n\tt de Student: ",r_tstud,sep="")
cat("\n\tt de Welch: ",r_twelch,sep="")
cat("\n\tU de Mann-Whitney: ",r_utrad,sep="")
cat("\n\nDiferencas:\n")
i2 <- 1
for (i in 1:3)
{
  cat("\nTeste da diferença de proporcao de sucessos:")
  cat(paste0("\n\tH0: p(",pares[i2+1],")-p(",pares[i2],") =  0"))
  cat(paste0("\n\tH1: p(",pares[i2+1],")-p(",pares[i2],") <> 0"))
  cat("\n")
  bintest <- DescTools::BinomDiffCI(a[i]+b[i],numamostras,a[i]+c[i],numamostras)
  print(bintest)

  cat(paste0("\nConclusão: a proporção de rejeições corretas pelo ",
      pares[i2+1]," é "))
  if(bintest[2]>0 & bintest[3]>0 )
  {
    cat("superior")
  } 
  if(bintest[2]<0 & bintest[3]<0 )
  {
    cat("inferior")
  } 
  if(bintest[2]<0 & bintest[3]>0 )
  {
    cat("indistinguível")
  }
  cat(paste0(" à do ",pares[i2],"."))
  cat("\n")
  
  i2 <- i2+2
} # for i

d_utrad <- density(c_utrad,na.rm=TRUE)
d_tstud <- density(c_tstud,na.rm=TRUE)
d_twelch <- density(c_twelch,na.rm=TRUE)
ymax <- max(c(d_tstud$y,d_twelch$y,d_utrad$y),na.rm=TRUE)
plot (NA,
      main="Distribuição dos valores p obtidos",
      xlab="valor p", ylab="Densidade",
      xlim=c(0,1), ylim=c(0,ymax), axes=FALSE)
axis(1)
axis(2)
abline(v=alpha,lty=2)
lines(d_twelch,lwd=2, lty=1, col="black")
lines(d_tstud,lwd=2, lty=2, col="black")
lines(d_utrad,lwd=2, lty=4, col="black")
legend("right",
       c(paste0("t Welch (rej.",round(r_twelch*100,1),"%)"), 
         paste0("t Student (rej.",round(r_tstud*100,1),"%)"), 
         paste0("U Mann-Whitney (rej.",round(r_utrad*100,1),"%)")),
       lty=c(1,2,4),
       lwd=2,
       col=c("black","black","black"),
       box.lwd=0, bg="transparent")

plot (NA,
      main="(detalhe, abaixo de p=0.05)",
      xlab="valor p", ylab="Densidade",
      xlim=c(0,0.055), ylim=c(2,13), axes=FALSE)
axis(1)
axis(2)
abline(v=alpha,lty=2)
lines(d_twelch,lwd=2, lty=1, col="black")
lines(d_tstud,lwd=2, lty=2, col="black")
lines(d_utrad,lwd=2, lty=4, col="black")
legend("topright",
       c(paste0("t Welch (rej.",round(r_twelch*100,1),"%)"), 
         paste0("t Student (rej.",round(r_tstud*100,1),"%)"), 
         paste0("U Mann-Whitney (rej.",round(r_utrad*100,1),"%)")),
       lty=c(1,2,4),
       lwd=2,
       col=c("black","black","black"),
       box.lwd=0, bg="transparent")
