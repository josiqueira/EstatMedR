source("eiras.create.population.R")

# s <- round(runif(1,1,1e5)) 
# print(s)
s <- 27863
set.seed(s)

pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(3, 8, 14),
                          sd=c(2.1,2.2,2.4))
mean1 <- mean(pop1)
s.mean1 <- sd(pop1)
median1 <- median(pop1)
pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(5, 12, 15),
                          sd=c(2.5,2.7,3.1))
mean2 <- mean(pop2)
s.mean2 <- sd(pop1)
median2 <- median(pop2)

col1 <- "#000000"
col2 <- eiras::FriendlyColor(8)

denspop1 <- density(pop1)
plot(denspop1,xlim=c(-2,30),ylim=c(0,0.25))
abline(v=mean1)
abline(v=median1,lty=2)
denspop2 <- density(pop2)
lines(denspop2,col=col2,lwd=2)
abline(v=mean2,col=col2,lwd=2)
abline(v=median2,lty=2,col=col2,lwd=2)
# normais
x1 <- seq(mean1-4*s.mean1, mean1+4*s.mean1, length.out=150)
y1 <- dnorm(x1,mean=mean1,sd=s.mean1)
lines(x1,y1,col=col1t,lwd=3)
x2 <- seq(mean2-4*s.mean2, mean1+4*s.mean2, length.out=150)
y2 <- dnorm(x2,mean=mean2,sd=s.mean2)
lines(x2,y2,col=col2t,lwd=3)

cat("\n\npop 1:")
cat(paste0("\n\tmean=",mean1))
cat(paste0("\n\tmedian=",median1))
cat("\n\npop 2:")
cat(paste0("\n\tmean=",mean2))
cat(paste0("\n\tmedian=",median2))

cat("\nTeste de normalidade (na populacao):\n")
print(swtp1 <- shapiro.test(sample(pop1,size=5000)))
print(swtp2 <- shapiro.test(sample(pop2,size=5000)))

# tentando várias amostras
n1 <- 5
n2 <- 8
col1t <- paste0(col1,"20")
col2t <- paste0(col2,"20")
plot(NA,xlim=c(-2,30),ylim=c(0,0.25))

for (i in 1:1500)
{
  am1 <- sample(pop1,size=n1)
  am2 <- sample(pop2,size=n2)
  d1 <- density(am1)
  lines(d1,lwd=0.1,col=col1t)
  d2 <- density(am2)
  lines(d2,lwd=0.1,col=col2t)
  if (i==1)
  {
    x1 <- d1$x
    m1 <- d1$y
    x2 <- d2$x
    m2 <- d2$y
  } else
  {
    m1 <- ((i-1)*m1+d1$y)/(i)
    m2 <- ((i-1)*m2+d2$y)/(i)
  }
  
}
lines(x1,m1,lwd=2,col=col1)
lines(x2,m2,lwd=2,col=col2)

rm(.Random.seed, envir=globalenv()) # reset seed
# s <- round(runif(1,1,1e5))
# cat("\n\nnewseed =",s,"\n")
s <- 64338 
set.seed(s)

col1t <- paste0(col1,"30")
col2t <- paste0(col2,"30")
amostra1 <- sample(pop1,size=n1)
cat("\n\nAmostra 1 (n=",n1,")\n",sep="")
a.mean1 <- mean(amostra1)
s.mean1 <- sd(amostra1)
a.median1 <- median(amostra1)
print(amostra1)
cat(paste0("\n\tmean=",a.mean1))
cat(paste0("\n\tmedian=",a.median1))
cat("\n\nAmostra 2 (n=",n2,")\n",sep="")
amostra2 <- sample(pop2,size=n2)
a.mean2 <- mean(amostra2)
s.mean2 <- sd(amostra2)
a.median2 <- median(amostra2)
print(amostra2)
cat(paste0("\n\tmean=",a.mean2))
cat(paste0("\n\tmedian=",a.median2))

plot(NA,xlim=c(-2,30),ylim=c(0,0.25),col=col1t)
# lines(denspop1,col=col1t,lwd=1)
# abline(v=mean1,col=col1t)
# abline(v=median1,lty=2,col=col1t)
# lines(denspop2,col=col2t,lwd=1)
# abline(v=mean2,col=col2t,lwd=1)
# abline(v=median2,lty=2,col=col2t,lwd=1)
dens1 <- density(amostra1)
lines(dens1,col=col1,lwd=2)
abline(v=a.mean1,col=col1)
abline(v=a.median1,lty=2,col=col1)
dens2 <- density(amostra2)
lines(dens2,col=col2,lwd=2)
abline(v=a.mean2,col=col2)
abline(v=a.median2,lty=2,col=col2)
# normais
x1 <- seq(a.mean1-4*s.mean1, a.mean1+4*s.mean1, length.out=150)
y1 <- dnorm(x1,mean=a.mean1,sd=s.mean1)
lines(x1,y1,col=col1t,lwd=3)
x2 <- seq(a.mean2-4*s.mean2, a.mean1+4*s.mean2, length.out=150)
y2 <- dnorm(x2,mean=a.mean2,sd=s.mean2)
lines(x2,y2,col=col2t,lwd=3)

cat("\nTeste de normalidade:\n")
print(swt1 <- shapiro.test(amostra1))
print(swt2 <- shapiro.test(amostra2))

cat("\nTeste t de Student:\n")
print(t <- t.test(amostra1,amostra2,var.equal=TRUE))

cat("\nTeste t de Welch/Satterthwite:\n")
print(t <- t.test(amostra1,amostra2))

cat("\nTeste U de Mann-Whitney Convencional:\n")
print(wilcox.test(amostra1,amostra2, exact = FALSE))

cat("\nTeste B de Brunner-Munzel:\n")
print(lawstat::brunner.munzel.test(amostra1,amostra2))

# Hollander & Wolfe, p. 110, results p. 111 and p. 126
cat("\nTeste U de Mann-Whitney Exato:\n")
datafrm <- data.frame(
  treino = c(amostra1,amostra2),
  grupo = factor(rep(c("amostra1", "amostra2"),
                     c(length(amostra1), length(amostra2))))
)
print(coin::wilcox_test(treino ~ grupo, data = datafrm,
                        distribution = "exact", conf.int = TRUE))
