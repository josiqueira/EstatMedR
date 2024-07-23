source("eiras.create.population.R")
source("eiras.shape.test.R")

s <- 27863
set.seed(s)
col1 <- "#000000"
col2 <- eiras::FriendlyColor(8)
col1t <- paste0(col1,"30")
col2t <- paste0(col2,"30")

pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(135, 160, 210),
                          sd=c(16,17,18))
pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(111, 186, 236),
                          sd=c(19,20,23))

s <- list()
s[["group1"]] <- pop1
s[["group2"]] <- pop2
shape <- shape.test(s, labels = c("Populacao 1", "Populacao 2"), echo=FALSE)

denspop1 <- density(pop1)
plot(denspop1,
     main="População\nDistribuição do colesterol total",
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),ylim=c(0,0.025),
     lwd=3, col=col1)
abline(v=shape[["anatomy.Populacao 1"]][2],lwd=1,lty=2,col=col1)
abline(v=shape[["anatomy.Populacao 1"]][3],lwd=1,lty=3,col=col1)
abline(v=shape[["anatomy.Populacao 1"]][4],lwd=1,lty=4,col=col1)
denspop2 <- density(pop2)
lines(denspop2,col=col2,lwd=3)
abline(v=shape[["anatomy.Populacao 2"]][2],lwd=1,lty=2,col=col2)
abline(v=shape[["anatomy.Populacao 2"]][3],lwd=1,lty=3,col=col2)
abline(v=shape[["anatomy.Populacao 2"]][4],lwd=1,lty=4,col=col2)
# normais
m <- shape[["anatomy.Populacao 1"]][2]
s <- shape[["anatomy.Populacao 1"]][13]
x1 <- seq(m-4*s, m+4*s, length.out=150)
y1 <- dnorm(x1,mean=m,sd=s)
lines(x1,y1,col=col1t,lwd=3)
m <- shape[["anatomy.Populacao 2"]][2]
s <- shape[["anatomy.Populacao 2"]][13]
x2 <- seq(m-4*s, m+4*s, length.out=150)
y2 <- dnorm(x2,mean=m,sd=s)
lines(x2,y2,col=col2t,lwd=3)
legend("topright",
       c("Normocolesterolêmicos","Hipercolesterolêmicos",
         "média","mediana","moda"),
       lty=c(1,1,2,3,4),
       lwd=c(3,3,1,1,1),
       col=c(col1,col2,"black","black","black"),
       box.lwd=0, bg="transparent")

s <- list()
s[["group1"]] <- pop1
s[["group2"]] <- pop2
shape <- shape.test(s, labels = c("Populacao 1", "Populacao 2"))
