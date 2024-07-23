mu <- 173
sigma <- 7
alfa <- 0.05

estatura <- c(169, 174, 175, 186)

# estatistica
n <- length(estatura)
media <- mean(estatura)
ep <- sigma / sqrt(n)
z <- (media-mu)/ep

# normal padronizada da populacao
range <- (max(4,abs(z)+4))
xpopp <- seq(-range, range, length.out=1000)
ypopp <- dnorm(xpopp, mean=0, sd=1)
xmin <- min(xpopp)
xmax <- max(xpopp)
ymin <- min(ypopp)
ymax <- max(ypopp)
plot(xpopp, ypopp, 
     main="Distribuição amostral padronizada\ncentrada em H0",
     xlab="Medias amostrais padronizadas da estatura",
     ylab="Densidade",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax),
     type="l",
     lwd=2, col="black")
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
points(-z,ymax/20,pch=16)
text(-z,ymax/20,"-z",pos=3,cex=0.8)

# hachura das area alem da media sob H0
xesq <- xpopp[which(xpopp<=-abs(z))]
yesq <- ypopp[which(xpopp<=-abs(z))]
lines(xesq,yesq,lwd=3,col="#008800")
xesq <- c(min(xesq),xesq,max(xesq))
yesq <- c(   0     ,yesq,   0     )
polygon(xesq,yesq,border=NA,col="#00880044")
xdir <- xpopp[which(xpopp>=abs(z))]
ydir <- ypopp[which(xpopp>=abs(z))]
lines(xdir,ydir,lwd=3,col="#008800")
xdir <- c(min(xdir),xdir,max(xdir))
ydir <- c(   0     ,ydir,   0     )
polygon(xdir,ydir,border=NA,col="#00880044")

