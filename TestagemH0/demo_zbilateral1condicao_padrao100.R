mu <- 173
sigma <- 7
alfa <- 0.05

set.seed(3)
estatura <- rnorm(mean=176, sd=sigma, n=99)
v <- 100*176-99*mean(estatura)
estatura <- c(estatura, v)

cat("População:")
cat("\n\tmu = ",mu, sep="")
cat("\n\tsigma = ",sigma, sep="")

# estatistica
cat("\nAmostra:")
n <- length(estatura)
cat("\n\tn = ",n, sep="")
media <- mean(estatura)
cat("\n\tmédia = ",media, sep="")
ep <- sigma / sqrt(n)
cat("\n\tep = ",ep, sep="")

cat("\nDistancia padronizada entre medias amostral e populacional:")
z <- (media-mu)/ep
cat("\n\tz = ",z, sep="")

# normal padronizada da populacao
xpopp <- seq(min(-4,z-4), max(4,z+4), length.out=1000)
ypopp <- dnorm(xpopp, mean=0, sd=1)
xmin <- min(xpopp)
xmax <- max(xpopp)
ymin <- min(ypopp)
ymax <- max(ypopp)
plot(xpopp, ypopp, 
     main="Distribuicao amostral padronizada\ncentrada em H0",
     xlab="Medias amostrais padronizadas da estatura",
     ylab="Densidade",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax),
     type="l",
     lwd=2, col="gray")
# hachura das caudas
z25 <- abs(qnorm(p=alfa/2, mean=0, sd=1)) # |z| em 2.5%
N95 = round(c(-z25, z25),2)
xesq <- xpopp[which(xpopp<=N95[1])]
yesq <- ypopp[which(xpopp<=N95[1])]
xesq <- c(min(xesq),xesq,max(xesq))
yesq <- c(   0     ,yesq,   0     )
polygon(xesq,yesq,border=NA,col="lightblue")
xdir <- xpopp[which(xpopp>=N95[2])]
ydir <- ypopp[which(xpopp>=N95[2])]
xdir <- c(min(xdir),xdir,max(xdir))
ydir <- c(   0     ,ydir,   0     )
polygon(xdir,ydir,border=NA,col="lightblue")
# H0
points(0,ymax/20,pch=13)
text(0,ymax/20,"H0",pos=3,cex=0.8)
# media amostral
points(z,ymax/20,pch=16)
text(z,ymax/20,"z",pos=3,cex=0.8)
