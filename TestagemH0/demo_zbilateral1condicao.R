mu <- 173
sigma <- 7
alfa <- 0.05

estatura <- c(169, 174, 175, 186)
n <- length(estatura)

# estatistica
cat("Amostra:")
media <- mean(estatura)
cat("\n\tmédia = ",media, sep="")
ep <- sigma / sqrt(n)
cat("\n\tep = ",ep, sep="")
z <- abs(qnorm(p=alfa/2, mean=0, sd=1)) # |z| em 2.5%
IC95 = round(c(media-z*ep, media+z*ep),2)
cat("\n\tIC95 = [",IC95[1],",",IC95[2],"]", sep="")

# normal populacional
xpop <- seq(mu-4*sigma, mu+4*sigma, length.out=1000)
ypop <- dnorm(xpop, mean=mu, sd=sigma)
# normal das medias amostrais (TCL)
xtcl <- seq(media-4*ep, media+4*ep, length.out=1000)
ytcl <- dnorm(xtcl, mean=media, sd=ep)
xmin <- min(xtcl)
xmax <- max(xtcl)
ymin <- min(ypop,ytcl)
ymax <- max(ypop,ytcl)
plot(xpop, ypop, 
     main="Distribuições e IC95% das médias amostrais",
     xlab="Estatura (cm)",
     ylab="Densidade",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax),
     type="l",
     lwd=3, col="#dddddd")
# hachura das caudas
xesq <- xtcl[which(xtcl<=IC95[1])]
yesq <- ytcl[which(xtcl<=IC95[1])]
xesq <- c(min(xesq),xesq,max(xesq))
yesq <- c(   0     ,yesq,   0     )
polygon(xesq,yesq,border=NA,col="pink")
xdir <- xtcl[which(xtcl>=IC95[2])]
ydir <- ytcl[which(xtcl>=IC95[2])]
xdir <- c(min(xdir),xdir,max(xdir))
ydir <- c(   0     ,ydir,   0     )
polygon(xdir,ydir,border=NA,col="pink")
# normal
lines(xtcl, ytcl,
      lwd=2, col="black")
# H0
points(mu,ymax/20,pch=13)
text(mu,ymax/20,"H0",pos=3,cex=0.8)
# media amostral
points(media,ymax/20,pch=16)
text(media,ymax/20,"m",pos=3,cex=0.8)
# IC95
lines(IC95,rep(ymax,2)/20)
points(IC95,rep(ymax,2)/20,pch="I")

legend("topright", 
       c("Populacional","Médias amostrais"), 
       col=c("#dddddd", "black"),
       lwd=c(3,2), 
       lty=1, 
       cex=0.7,
       box.lwd=0, bg="transparent")  


