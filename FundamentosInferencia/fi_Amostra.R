# amostra
set.seed(27) # comente esta linha se quiser variar a amostra
n <- 100
amostra <- round(rnorm(n,media,dp),1)
dens.am <- density(amostra)
plot(dens.am,
     lwd=2,
     main=paste0("Amostra (n=",n,")"),
     xlab="Hb (g/dl)", 
     ylab="Densidade",
     ylim=c(0,y.max), # mantendo a escala y
     col=friendlycolor(1),
     type="l")
# relato
am <- mean(amostra,na.rm=TRUE)
as <- sd(amostra,na.rm=TRUE)
v <- ""
v <- paste0(v,"Amostra:\n")
v <- paste0(v,"\tmedia amostral: ",round(am,4),"\n")
v <- paste0(v,"\td.p. amostral: ",round(as,4))
cat(v)
