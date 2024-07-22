# distribuicao dos desvios padrao das reamostras (deixando a escala livre)

# para o desvio padrao
plot.density.withmeansd(boot.sds,
                        main=paste0("Distribuicao dos desvios padrao das reamostras\n(bootstrapping, B=",B,")"),
                        xlab="Desvios padrao amostrais de Colesterol LDL (mg/dl)",
                        ylab="Densidade",
                        col=friendlycolor(29))
# ajustando distribuicao normal
um <- mean(amo.unique,na.rm=TRUE)
us <- sd(amo.unique,na.rm=TRUE)
sm <- mean(boot.sds,na.rm=TRUE)
eps <- sd(boot.sds,na.rm=TRUE)
x <- seq(sm-4*eps,sm+4*eps,length.out=100)
y <- dnorm(x, mean=sm, sd=eps)
lines(x,y,lty=2,lwd=2,col=friendlycolor(3))
# intervalo de confianca 95%
icx <- quantile(boot.sds,probs=c((1-0.95)/2,1-((1-0.95)/2)))
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
points(sm,icy,pch=21,cex=1.5,col="black",bg="black")
# media amo.uniquel (ponto solido)
points(us,icy-h,pch=21,cex=1.5,col=friendlycolor(3),bg=friendlycolor(3))
# dp populacional (ponto branco)
points(ps,icy-2*h,pch=21,cex=1.5,col=friendlycolor(7),bg="white")

# relato
v <- ""
v <- paste0(v,"\n")
v <- paste0(v,"\terro padrao do desvio padrao: ",round(eps,4),"\n")
v <- paste0(v,"\tIC95(sigma): [",round(ic95s[1],4),",",round(ic95s[2],4),"]","\n")
cat(v)

