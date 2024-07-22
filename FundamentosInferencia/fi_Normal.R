media <- 11.9
dp <- 1.5
Hb <- seq(media-4*dp,media+4*dp,length.out=1e3)
dist.pop <- dnorm(Hb,mean=media,sd=dp)
plot(Hb,
     dist.pop,
     lwd=5,
     main="População hipotética",
     xlab="Hb (g/dl)", 
     ylab="Densidade",
     col=friendlycolor(7),
     type="l")
abline(v=c(media-2*dp,
           media-dp,
           media,
           media+dp,
           media+2*dp),
       lty=2,
       col=friendlycolor(7))
# relato
v <- ""
v <- paste0(v,"Populacao:\n")
v <- paste0(v,"\tmedia populacional: ",media,"\n")
v <- paste0(v,"\td.p. populacional: ",dp,"\n")
cat(v)
