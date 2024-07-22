source("eiras.friendlycolor.R")

# print(seed <- runif(1,1,1e6))
seed <- 392075
set.seed(seed)
# anemicos
anemicas <- rnorm(809, mean=12.972, sd=1.5)
saudaveis <- rnorm(1770, mean=18.246, sd=3)
cutoff <- 16
da <- density(anemicas)
ds <- density(saudaveis)
cola <- friendlycolor(8);  cola.t<- paste(cola,"88",sep="")
cols <- friendlycolor(20); cols.t<- paste(cols,"88",sep="")
plot(da, 
     main="Distribuicao da Ferritina Serica",
     xlab="Ferritina", ylab="Densidade",
     xlim=c(0,30),ylim=c(0,max(da$y, ds$y)),
     col=cola,lwd=2)
lines(ds, lty=2,col=cols,lwd=2)
abline(v=cutoff,lty=4)
# separa falso negativo
nfneg <- sum(anemicas>cutoff)
fneg <- data.frame(da$x[da$x>cutoff],da$y[da$x>cutoff]); names(fneg) <- c("x","y")
nfpos <- sum(saudaveis<cutoff)
fpos <- data.frame(ds$x[ds$x<cutoff],ds$y[ds$x<cutoff]); names(fpos) <- c("x","y")
# hachura
x <- fneg$x; x <- c(min(x),x,max(x))
y <- fneg$y; y <- c(     0,y,0     )
polygon(x,y,col=cola.t,border=NA)
x <- fpos$x; x <- c(min(x),x,max(x))
y <- fpos$y; y <- c(     0,y,0     )
polygon(x,y,col=cols.t,border=NA)
legend("topleft",
       c("anemicas", "saudaveis",
         paste("falso neg. (",nfneg,")",sep=""),
         paste("falso pos (",nfpos,")",sep="")
       ),
       col=c(cola,cols,cola.t,cols.t),
       lwd=c(2,2,10,10),
       lty=c(1,2,1,1),
       box.lwd=0, bg="transparent"  
)
