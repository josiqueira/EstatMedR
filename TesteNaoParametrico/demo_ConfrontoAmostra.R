source("eiras.shape.test.R")
source("eiras.create.population.R")

s <- 27863
set.seed(s)
pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(135, 160, 210),
                          sd=c(16,17,18))
pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(111, 186, 236),
                          sd=c(19,20,23))

rm(.Random.seed, envir=globalenv()) # reset seed
# s <- round(runif(1,1,1e5))
# cat("\n\nnewseed =",s,"\n")
s <- 60789
set.seed(s)

n1 <- 6
n2 <- 8
col1 <- "#000000"
col2 <- eiras::FriendlyColor(8)
col1t <- paste0(col1,"30")
col2t <- paste0(col2,"30")
amostra1 <- sample(pop1,size=n1)
amostra2 <- sample(pop2,size=n2)
s <- list()
s[["group1"]] <- amostra1
s[["group2"]] <- amostra2
shape <- shape.test(s, labels = c("Amostra 1", "Amostra 2"))

densamo1 <- density(amostra1)
plot(densamo1,
     main=paste0("Par de Amostras (n1=",n1,", n2=",n2,"),",
                 " comparando com normais (ao fundo)\n",
                 "Distribuição do colesterol total"),
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),ylim=c(0,0.025),
     lwd=3, col=col1)
abline(v=shape[["anatomy.Amostra 1"]][2],lwd=1,lty=2,col=col1)
abline(v=shape[["anatomy.Amostra 1"]][3],lwd=1,lty=3,col=col1)
abline(v=shape[["anatomy.Amostra 1"]][4],lwd=1,lty=4,col=col1)
densamo2 <- density(amostra2)
lines(densamo2,col=col2,lwd=3)
abline(v=shape[["anatomy.Amostra 2"]][2],lwd=1,lty=2,col=col2)
abline(v=shape[["anatomy.Amostra 2"]][3],lwd=1,lty=3,col=col2)
abline(v=shape[["anatomy.Amostra 2"]][4],lwd=1,lty=4,col=col2)
# normais
m <- shape[["anatomy.Amostra 1"]][2]
s <- shape[["anatomy.Amostra 1"]][13]
x1 <- seq(m-4*s, m+4*s, length.out=150)
y1 <- dnorm(x1,mean=m,sd=s)
lines(x1,y1,col=col1t,lwd=3)
m <- shape[["anatomy.Amostra 2"]][2]
s <- shape[["anatomy.Amostra 2"]][13]
x2 <- seq(m-4*s, m+4*s, length.out=150)
y2 <- dnorm(x2,mean=m,sd=s)
lines(x2,y2,col=col2t,lwd=3)
