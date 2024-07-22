# amostra
set.seed (27) # comente esta linha se quiser variar a amostra
n <- 100
amostra <- round(rnorm(n,media,dp),1)
# populacao
plot(Hb,dist.pop,lwd=10,
     main=paste0("População hipotética e amostra (n=",n,")"),
     xlab="Hb (g/dl)", ylab="Densidade",
     col=paste0(friendlycolor(7),40),type="l")
# amostra
dens.am <- density(amostra)
lines(dens.am, lwd=2, col=friendlycolor(1))
legend("right", 
       c("Populacao",paste0("Amostra, n=",n)), 
       col=c(paste0(friendlycolor(7),40),friendlycolor(1)),
       lwd=c(10,2), 
       lty=c(1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  
# guardando o maximo para uso adiante
y.max <- max(dist.pop,dens.am$y,na.rm=TRUE) 

# relato
am <- mean(amostra,na.rm=TRUE)
as <- sd(amostra,na.rm=TRUE)
v <- ""
v <- paste0(v,"Populacao:\n")
v <- paste0(v,"\tmedia populacional: ",media,"\n")
v <- paste0(v,"\td.p. populacional: ",dp,"\n")
v <- paste0(v,"\n")
v <- paste0(v,"Amostra:\n")
v <- paste0(v,"\tmedia amostral: ",round(am,4),"\n")
v <- paste0(v,"\td.p. amostral: ",round(as,4))
cat(v)
