source("eiras.shade.polygon.R")
source("eiras.create.population.R")

if(exists("multi")==0) {multi<-1}
pdf(paste0("tmpTCL_",multi,".pdf"),page="a4")

s <- 27863
set.seed(s)
col1 <- eiras::FriendlyColor(21)
col2 <- eiras::FriendlyColor(10)
col1t <- paste0(col1,"70")
col2t <- paste0(col2,"70")
col1e <- eiras::FriendlyColor(21-2)
col2e <- eiras::FriendlyColor(10-2)
n1 <- 6*multi
n2 <- 8*multi
B <- 5000
maxy <- 0.065

pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(135, 160, 210),
                          sd=c(16,17,18))
mean1 <- mean(pop1)
spop1 <- sd(pop1)
median1 <- median(pop1)
iqr1 <- IQR(pop1, na.rm=TRUE)

pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(111, 186, 236),
                          sd=c(19,20,23))
mean2 <- mean(pop2)
spop2 <- sd(pop2)
median2 <- median(pop2)
iqr2 <- IQR(pop2, na.rm=TRUE)

denspop1 <- density(pop1)
mode1 <- denspop1$x[which.max(denspop1$y)]
plot(denspop1,
     main="População\nDistribuição do colesterol total",
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),ylim=c(0,maxy),
     lwd=3, col=col1)
abline(v=mean1,lwd=1,lty=2,col=col1)
denspop2 <- density(pop2)
mode2 <- denspop2$x[which.max(denspop2$y)]
lines(denspop2,col=col2,lwd=3)
abline(v=mean2,lwd=1,lty=2,col=col2)
# normais
x1 <- seq(mean1-4*spop1, mean1+4*spop1, length.out=150)
y1 <- dnorm(x1,mean=mean1,sd=spop1)
lines(x1,y1,col=col1e,lty=2)
x2 <- seq(mean2-4*spop2, mean1+4*spop2, length.out=150)
y2 <- dnorm(x2,mean=mean2,sd=spop2)
lines(x2,y2,col=col2e,lty=2)

# Amostras
rm(.Random.seed, envir=globalenv()) # reset seed
# s <- round(runif(1,1,1e5))
# cat("\n\nnewseed =",s,"\n")
s <- 18905
set.seed(s)
am1 <- sample(pop1,size=n1)
rm(.Random.seed, envir=globalenv()) # reset seed
# s <- round(runif(1,1,1e5))
# cat("\n\nnewseed =",s,"\n")
s <- 95300
set.seed(s)
am2 <- sample(pop2,size=n2)
# plot amostra sobre populacao
d1 <- density(am1)
# lines(d1,lwd=1,col=col1t)
d2 <- density(am2)
# lines(d2,lwd=1,col=col2t)

# localizacao e dispersao
am.mean1 <- mean(am1)
am.sd1 <- sd(am1)
am.median1 <- median(am1)
am.iqr1 <- IQR(am1, na.rm=TRUE)
am.mean2 <- mean(am2)
am.sd2 <- sd(am2)
am.median2 <- median(am2)
am.iqr2 <- IQR(am2, na.rm=TRUE)

plot(d1,
     main=paste0("Exemplo de Amostra (n1=",n1,", n2=",n2,")\n",
                 "Distribuição do colesterol total"),
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),ylim=c(0,maxy),
     lwd=3, col=col1)
abline(v=am.mean1,lwd=1,lty=2,col=col1)
denspop2 <- density(pop2)
lines(d2,col=col2,lwd=3)
abline(v=am.mean2,lwd=1,lty=2,col=col2)
shade.polygon(d1,mean=am.mean1,sd=am.sd1,col=col1,sds=2)
shade.polygon(d2,mean=am.mean2,sd=am.sd2,col=col2,sds=2)

# demo, B samples
plot(d1,
     main=paste0("Repetição de ",B," amostras",
                 " (n1=",n1,", n2=",n2,")\n",
                 "- 10 amostras enfatizadas -\n",
                 "Distribuição do colesterol total"),
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),ylim=c(0,maxy),
     lwd=3, col=col1t)
abline(v=am.mean1,lwd=1,lty=2,col=col1)
denspop2 <- density(pop2)
lines(d2,col=col2t,lwd=3)
abline(v=am.mean2,lwd=1,lty=2,col=col2)
b.am1 <- b.am2 <- c()
enf1.x <- enf2.x <- c()
enf1.y <- enf2.y <- c()
for (b in 1:B)
{
  # sample 1
  am1 <- sample(pop1,size=n1)
  d.tmp <- density(am1)
  lines(d.tmp,lwd=1,col=col1t)
  if(((b+1)%%(B/10))==0) 
  {
    enf1.x <- c(enf1.x,NA,d.tmp$x)
    enf1.y <- c(enf1.y,NA,d.tmp$y)
  }
  b.am1 <- c(b.am1,mean(am1))
  # sample 2
  am2 <- sample(pop2,size=n2)
  d.tmp <- density(am2)
  lines(d.tmp,lwd=1,col=col2t)
  if(((b+1)%%(B/10))==0) 
  {
    enf2.x <- c(enf2.x,NA,d.tmp$x)
    enf2.y <- c(enf2.y,NA,d.tmp$y)
  }
  b.am2 <- c(b.am2,mean(am2))
}
lines(enf1.x,enf1.y,lwd=2,col=col1e)
lines(enf2.x,enf2.y,lwd=2,col=col2e)

# TCL
b.d1 <- density(b.am1)
b.mean1 <- mean(b.am1)
b.sd1 <- sd(b.am1)
b.d2 <- density(b.am2)
b.mean2 <- mean(b.am2)
b.sd2 <- sd(b.am2)
plot(b.d1,
     main=paste0("Média da repetição de ",B," amostras",
                 " (n1=",n1,", n2=",n2,")\n",
                 "Distribuição da media do colesterol total"),
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),ylim=c(0,maxy),
     lwd=3, col=col1)
abline(v=b.mean1,lwd=1,lty=2,col=col1)
lines(b.d2,col=col2,lwd=3)
abline(v=b.mean2,lwd=1,lty=2,col=col2)
shade.polygon(b.d1,mean=b.mean1,sd=b.sd1,col=col1,sds=2)
shade.polygon(b.d2,mean=b.mean2,sd=b.sd2,col=col2,sds=2)
# normais
x1 <- seq(b.mean1-4*b.sd1, b.mean1+4*b.sd1, length.out=150)
y1 <- dnorm(x1,mean=b.mean1,sd=b.sd1)
lines(x1,y1,col=col1e,lty=2)
x2 <- seq(b.mean2-4*b.sd2, b.mean2+4*b.sd2, length.out=150)
y2 <- dnorm(x2,mean=b.mean2,sd=b.sd2)
lines(x2,y2,col=col2e,lty=2)

dev.off()