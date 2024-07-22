source("eiras.plot.density.withmeansd.R")
source("eiras.sampling.R")

# amostra
set.seed (27) # comente esta linha se quiser variar a amostra
media <- 11.9
dp <- 1.5
n <- 9
amostra <- round(rnorm(n,media,dp),1)

# bootstrapping 
B <- 1e5 # aumente o numero de reamostras para melhor resultado
limits <- plot.density.withmeansd(amostra,
                                main=paste("Amostra unica\n(bootstrapping com ",sprintf("%d",B)," reamostragens)",sep=""),
                                xlab="Hb (g/dl)",
                                ylab="Densidade",
                                col=friendlycolor(1),
                                getlimits = TRUE)
samples <- sampling(amostra,B=B,
                    n=n,replace=TRUE,
                    graph=TRUE,col=friendlycolor(29),
                    y.pos = limits[4]*1.25)
boot.medias <- samples[[2]]
boot.sds <- samples[[3]]
legend("right", 
       c("Amostra unica", 
         "Media am. +-3 dp",
         paste("bootstraps: ",B,sep=""),
         "Media boot +-3 dp"
       ), 
       col=c(friendlycolor(3),
             paste(friendlycolor(3),"88",sep=""),
             friendlycolor(29),
             paste(friendlycolor(29),"88",sep="")             
       ),
       lwd=c(2,10,2,10), 
       lty=c(1,1,1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  
# distribuicao da amostra original, sobreposta para referencia
plot.density.singleline(amostra,col=friendlycolor(1))

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

# relato
v <- ""
am <- mean(amostra)
as <- sd(amostra)
v <- paste0(v,"Amostra unica (n = ",n,"):\n")
v <- paste0(v,"\tmedia amostral: ",round(am,4),"\n")
v <- paste0(v,"\td.p. amostral: ",round(as,4))
bm <- mean(boot.medias)
bs <- mean(boot.sds)
ep <- sd(boot.medias)
ic95 <- quantile(boot.medias,probs=c(0.05/2, 1-(0.05/2)))
v <- paste0(v,"\nAmostras:",B," com n = ",n,"\n")
v <- paste0(v,"\tmedia das medias amostrais: ",round(bm,4),"\n")
v <- paste0(v,"\tmedia dos d.p. amostrais: ",round(bs,4),"\n")
v <- paste0(v,"\td.p. das medias amostrais: ",round(ep,4)," (erro padrao da media)","\n")
v <- paste0(v,"\tIC95: [",round(ic95[1],4),",",round(ic95[2],4),"]")
cat(v)
