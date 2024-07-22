# distribuicao das medias das reamostras (deixando a escala livre)
plot.density.withmeansd(boot.medias,
                        main="Distribuicao de medias das reamostras\n(bootstrapping)",
                        xlab="Medias amostrais de Hb (g/dl)",
                        ylab="Densidade",
                        col=friendlycolor(29))
# ajustando distribuicao normal
um <- mean(amostra,na.rm=TRUE)
us <- sd(amostra,na.rm=TRUE)
bm <- mean(boot.medias,na.rm=TRUE)
ep <- sd(boot.medias,na.rm=TRUE)
x <- seq(bm-4*ep,bm+4*ep,length.out=100)
y <- dnorm(x, mean=bm, sd=ep)
lines(x,y,lty=2,lwd=2,col=friendlycolor(3))
# intervalo de confianca 95%
icx <- quantile(boot.medias,probs=c((1-0.95)/2,1-((1-0.95)/2)))
xx1 <- x-icx[1]
xx2 <- x-icx[2]
icy <- c(NA,NA)
icy[1] <- mean(y[which(abs(xx1)==min(abs(xx1),na.rm=TRUE))],na.rm=TRUE)
icy[2] <- mean(y[which(abs(xx2)==min(abs(xx2),na.rm=TRUE))],na.rm=TRUE)
icy <- mean(icy,na.rm=TRUE)
h <- icy/5
lines(c(icx[1],icx[1],icx[1],icx[2],icx[2],icx[2]),
      c(icy-h ,icy+h ,icy   ,icy   ,icy+h ,icy-h ),
      lwd=2, col="black")
# media das reamostras (ponto solido)
points(bm,icy,pch=21,cex=1.5,col="black",bg="black")
# media amostral (ponto solido)
points(um,icy-h,pch=21,cex=1.5,col=friendlycolor(3),bg=friendlycolor(3))
# media populacional (ponto branco)
points(media,icy-2*h,pch=21,cex=1.5,col=friendlycolor(7),bg="white")
