# s <- round(runif(1,1,10000))
# print(s)
set.seed(8199)

library(DescTools)

source("eiras.friendlycolor.R")

# side <- c("less", "two.sided", "greater")
if (!exists("side"))
{
  side <- "two.sided"
}
if (is.na(side))
{
  side <- "two.sided"
}

n <- 30
B <- 1e5
alfa <- .05

proppop <- .1
amostra <- rbinom(n=n,size=1,prob=proppop)
sucessos <- sum(amostra)
m <- mean(amostra)
s <- sd(amostra)
e <- sd(amostra)/sqrt(length(amostra))
a <- DescTools::Skew(amostra)
d.amostra <- table(amostra)
d.amostra <- d.amostra/sum(d.amostra)
titulo <- paste("Probabilidades (p = ",proppop,", n = ",n,")\nIC 95% ",sep="")
if (side=="less") {titulo <- paste(titulo,"unilateral à esquerda",sep="")}
if (side=="two.sided") {titulo <- paste(titulo,"bilateral",sep="")}
if (side=="greater") {titulo <- paste(titulo,"unilateral à direita",sep="")}
plot(d.amostra, 
     main=titulo,
     ylim=c(0,1.09),
     xlab="Evento", ylab="Proporção", axes=FALSE)
axis(2, at=seq(0,1,length.out = 11))
axis(1, at=c(0,1), labels=c("ausente","presente"))

# Relatorio

cat("\n")
cat("Amostra:\n")
cat(" ",sucessos," sucessos em ",n," tentativas\n")
cat(" media: ",format(round(m,4),nsmall=4),"\n")
cat(" desvio-padrão: ",format(round(s,4),nsmall=4),"\n")
cat(" erro-padrao: ",format(round(e,4),nsmall=4),"\n")
cat(" assimetria: ",format(round(a,4),nsmall=4),"\n")

cat("\n")
cat("Intervalos:\n")
if (side=="two.sided")
{
  q <- quantile(replicate(B, mean(sample(amostra, replace=TRUE))), 
                probs=c(alfa/2,1-alfa/2))
}
if (side=="less")
{
  q <- quantile(replicate(B, mean(sample(amostra, replace=TRUE))), 
                probs=c(0,1-alfa))
  q[1] <- NA
}
if (side=="greater")
{
  q <- quantile(replicate(B, mean(sample(amostra, replace=TRUE))), 
                probs=c(alfa,1))
  q[2] <- NA
}
txt.q <- paste("[",format(round(q[1],4),nsmall=4),",",format(round(q[2],4),nsmall=4),"]",sep="")
cat("IC95 bootstrapping:\n",txt.q,"\n")
cat("\n")
b <- binom.test(sum(amostra), n=n, alternative=side)$conf.int
if (side=="less"){b[1] <- NA}
if (side=="greater"){b[2] <- NA}
txt.b <- paste("[",format(round(b[1],4),nsmall=4),",",format(round(b[2],4),nsmall=4),"]",sep="")
cat("IC95 teste binomial:\n",txt.b,"\n")
cat("\n")
t <- t.test(amostra, mu=proppop, alternative=side)$conf.int
if (side=="less"){t[1] <- NA}
if (side=="greater"){t[2] <- NA}
txt.t <- paste("[",format(round(t[1],4),nsmall=4),",",format(round(t[2],4),nsmall=4),"]",sep="")
cat("IC95 teste t:\n",txt.t,"\n")

# intervalos no grafico
if (side=="greater")
{
  to <- 3*m
  if(to > 1) {to <- 1} 
}
if (side=="less")
{
  to <- 0
}
col.q <- friendlycolor(2)
x <- 0.4
lines(rep(x,2),q,lty=1,col=col.q)
if (side=="two.sided") {lines(rep(x,2),q,lty=1,col=col.q)} else
{
  if(!is.na(q[1])) {arrows(x,q[1],x,to,col=col.q,length=0.1, angle=30)}
  if(!is.na(q[2])) {arrows(x,q[2],x,0,col=col.q,length=0.1, angle=30)}
}
points(rep(x,2),q,pch="-",col=col.q)
points(x,m,pch=21,col=col.q,bg=col.q,cex=0.5)

col.b <- friendlycolor(8)
x <- 0.5
lines(rep(x,2),b,lty=1,col=col.b)
if (side=="two.sided") {lines(rep(x,2),b,lty=1,col=col.b)} else
{
  if(!is.na(b[1])) {arrows(x,b[1],x,to,col=col.b,length=0.1, angle=30)}
  if(!is.na(b[2])) {arrows(x,b[2],x,0,col=col.b,length=0.1, angle=30)}
}
points(rep(x,2),b,pch="-",col=col.b)
points(x,m,pch=21,col=col.b,bg=col.b,cex=0.5)

col.t <- friendlycolor(14)
x <- 0.6
pch <- rep("-",2)
if (side=="two.sided") {lines(rep(x,2),t,lty=1,col=col.t)} else
{
  if(!is.na(t[1])) {arrows(x,t[1],x,to,col=col.t,length=0.1, angle=30)}
  if(!is.na(t[2])) {arrows(x,t[2],x,0,col=col.t,length=0.1, angle=30)}
}
points(rep(x,2),t,pch=pch,col=col.t)
points(x,m,pch=21,col=col.t,bg=col.t,cex=0.5)

# media populacional
abline(h=proppop, lty=2, col="#222222")

legend("top", 
       c(
         paste("Bootstrapping",txt.q), 
         paste("Binomial",txt.b), 
         paste("teste t",txt.t), 
         "Prop. pop. (p)"), 
       col=c(col.q, col.b, col.t, "black"),
       lwd=1, 
       lty=c(1,1,1,2), 
       cex=0.7,
       box.lwd=0, bg="transparent")  
