source("eiras.shape.test.R")
source("eiras.create.population.R")

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

rm(.Random.seed, envir=globalenv()) # reset seed
# s <- round(runif(1,1,1e5))
# cat("\n\nnewseed =",s,"\n")
s <- 8336
set.seed(s)

# tentando várias amostras
numamostras <- 1500
n1 <- 6
n2 <- 8
plot(NA,
     main=paste0("Amostras (",numamostras," tentativas) em média (linhas sólidas)",
                 "\n","n1 = ",n1,", n2 = ",n2),
     xlab="Colesterol total (mg/dl)", ylab="Densidade",
     xlim=c(0,350),
     ylim=c(0,0.025))
for (i in 1:numamostras)
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
