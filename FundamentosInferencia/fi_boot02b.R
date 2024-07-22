# distribuicao dos dp das reamostras

defaultW <- getOption("warn")
options(warn = -1)

plot.density.withmeansd(boot.sds,
                        main="Distribuicao de desvios padrao das reamostras\n(bootstrapping)",
                        xlab="Desvios padrao amostrais de Hb (g/dl)",
                        ylab="Densidade",
                        col=friendlycolor(29))

# ajustando distribuicao normal
bm <- mean(boot.medias,na.rm=TRUE)
bs <- mean(boot.sds,na.rm=TRUE)
eps <- sd(boot.sds,na.rm=TRUE)
x <- seq(bs-4*eps,bs+4*eps,length.out=100)
y <- dnorm(x, mean=bs, sd=eps)
lines(x,y,lty=2,lwd=2,col=friendlycolor(3))
# intervalo de confianca 95%
icx <- quantile(boot.sds,probs=c(0.05/2,1-(0.05/2)))
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
# media dos dp das reamostras (ponto solido)
points(bs,icy,pch=21,cex=1.5,col="black",bg="black")
# dp amostral (ponto solido)
points(um,icy-h,pch=21,cex=1.5,col=friendlycolor(3),bg=friendlycolor(3))
# media populacional (ponto branco)
points(dp,icy-2*h,pch=21,cex=1.5,col=friendlycolor(7),bg="white")

# relato
v <- ""
v <- paste0(v,"Amostra unica (n = ",n,"):\n")
v <- paste0(v,"\tmedia amostral: ",round(am,4),"\n")
v <- paste0(v,"\td.p. amostral: ",round(as,4))
ic95s <- quantile(boot.sds,probs=c(0.05/2, 1-(0.05/2)))
v <- paste0(v,"\nAmostras:",B," com n = ",n,"\n")
v <- paste0(v,"\tmedia das medias amostrais: ",round(bm,4),"\n")
v <- paste0(v,"\tmedia dos d.p. amostrais: ",round(bs,4),"\n")
v <- paste0(v,"\td.p. dos desvios padrao amostrais: ",round(eps,4)," (erro padrao do desvio padrao)","\n")
v <- paste0(v,"\tIC95(sigma): [",round(ic95s[1],4),",",round(ic95s[2],4),"]")
cat(v)

options(warn = defaultW)
