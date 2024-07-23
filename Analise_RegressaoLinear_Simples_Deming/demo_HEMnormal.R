Gestantes <- readRDS("Gestante.rds")
densidade <- density(Gestantes$HEM)
media <- mean(Gestantes$HEM)
desvpad <- sd(Gestantes$HEM)
x <- seq(media-3*desvpad, 
         media+3*desvpad, length.out=100)
y <- dnorm(x, mean=media, sd=desvpad)
plot(densidade,
     xlim=c(min(x),max(x)),
     ylim=c(0, max(y,densidade$y,na.rm=TRUE)),
     xlab="Hemacias (milhoes/mm^3)",
     ylab="densidade",
     type="l")
lines(x,y,lty=2)
