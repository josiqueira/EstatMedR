# Demonstration of sampling and bootstrapping

source("eiras.friendlycolor.R")
source("eiras.create.population.R")
source("eiras.plot.density.withmeansd.R")
source("eiras.sampling.R")

# create a simulated population
population <- create.population(kind="normal", round=0, 
                  n=c(2000,6000,3500,3000),
                  param1=c(160,200,230,280), # mean
                  param2=c(5,10,30,15) # sd
                  )
limits <- plot.density.withmeansd(population,
                        main="Populacao simulada\n(sampling)",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        col=friendlycolor(7), getlimits=TRUE)
B <- 10000
n <- 36
samples <- sampling(population,B=B,
                    n=n,replace=FALSE,
                    graph=TRUE,col=friendlycolor(3))
amo.medias <- samples[[2]]
amo.sds <- samples[[3]]
legend("right", 
       c("Distribuicao", 
         "Media pop +-3 d.p.",
         paste("amostras: ",B,sep=""),
         "Media am. +-3 d.p."
         ), 
       col=c(friendlycolor(7),
             paste(friendlycolor(7),"88",sep=""),
             friendlycolor(3),
             paste(friendlycolor(3),"88",sep="")             
             ),
       lwd=c(2,10,2,10), 
       lty=c(1,1,1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  
# distribuicao da populacao original, sobreposta para referencia
plot.density.singleline(population,col=friendlycolor(7))

pm <- mean(population,na.rm=TRUE)
ps <- sd(population,na.rm=TRUE)
am <- mean(amo.medias,na.rm=TRUE)
as <- mean(amo.sds,na.rm=TRUE)
v <- ""
v <- paste(v,"Populacao:\n")
v <- paste(v,"\tmedia populacional:",round(pm,4),"\n")
v <- paste(v,"\td.p. populacional:",round(ps,4),"\n")
v <- paste(v,"\n")
v <- paste(v,"Amostras:",B,"com n =",n,"\n")
v <- paste(v,"\tmedia das medias amostrais:",round(am,4),"\n")
v <- paste(v,"\tmedia dos d.p. amostrais:",round(as,4))
cat(v)

# distribuicao das medias amostrais (mantendo a escala)
plot.density.withmeansd(amo.medias,
                        main="Distribuicao de medias amostrais\n(sampling)",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        x.min=limits[1], x.max=limits[2],
                        col=friendlycolor(3))

# distribuicao das medias amostrais (deixando a escala livre)
plot.density.withmeansd(amo.medias,
                        main="Distribuicao de medias amostrais\n(sampling)",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        col=friendlycolor(3))
# ajustando distribuicao normal
am <- mean(amo.medias,na.rm=TRUE)
ep <- sd(amo.medias,na.rm=TRUE)
x <- seq(am-4*ep,am+4*ep,length.out=100)
y <- dnorm(x, mean=am, sd=ep)
lines(x,y,lty=2,lwd=2,col=friendlycolor(3))
# intervalo de confianca 95%
icx <- quantile(amo.medias,probs=c((1-0.95)/2,1-((1-0.95)/2)))
icy <- mean(y[which(round(x,0)==round(icx,0))],na.rm=TRUE)
h <- icy/5
lines(c(icx[1],icx[1],icx[1],icx[2],icx[2],icx[2]),
      c(icy-h ,icy+h ,icy   ,icy   ,icy+h ,icy-h ),
      lwd=2, col=friendlycolor(3))
# media amostral (ponto solido)
points(am,icy,pch=21,cex=1.5,col=friendlycolor(3),bg=friendlycolor(3))
# media populacional (ponto branco)
points(pm,icy,pch=21,cex=1.5,col=friendlycolor(3),bg="white")

am <- mean(amo.medias,na.rm=TRUE)
ep <- sd(amo.medias,na.rm=TRUE)
v <- ""
v <- paste(v,"Amostras:",B,"com n =",n,"\n")
v <- paste(v,"\tmedia das medias amostrais:",round(am,4),"\n")
v <- paste(v,"\td.p. das medias amostrais:",round(ep,4))
cat(v)


# preparando o bootstrapping
plot.density.withmeansd(population,
                        main="Populacao simulada\n(amostra unica)",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        x.min=limits[1], x.max=limits[2],
                        y.min=limits[3], y.max=limits[4],
                        col=friendlycolor(7))
B2 <- 1
samples <- sampling(population,B=B2,
                    n=n,replace=FALSE,
                    graph=TRUE,col=friendlycolor(1))
amo.unique <- samples[[1]]
legend("right", 
       c("Distribuicao", 
         "Media pop +-3 d.p.",
         paste("amostras: ",B,sep=""),
         "Media am. +-3 d.p."
       ), 
       col=c(friendlycolor(7),
             paste(friendlycolor(7),"88",sep=""),
             friendlycolor(1),
             paste(friendlycolor(1),"88",sep="")             
       ),
       lwd=c(2,10,2,10), 
       lty=c(1,1,1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  

# amostra unica
plot.density.withmeansd(amo.unique,
                        main="Amostra unica",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        x.min=limits[1], x.max=limits[2],
                        y.min=limits[3], y.max=limits[4],
                        col=friendlycolor(1))
legend("right", 
       c("Amostra", 
         "Media am. +-3 d.p."
       ), 
       col=c(friendlycolor(1),
             paste(friendlycolor(1),"88",sep="")             
       ),
       lwd=c(2,10), 
       lty=c(1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  

# bootstrapping
plot.density.withmeansd(amo.unique,
                        main="Amostra unica\n(bootstrapping)",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        x.min=limits[1], x.max=limits[2],
                        y.min=limits[3], y.max=limits[4],
                        col=friendlycolor(3))
samples <- sampling(amo.unique,B=B,
                    n=n,replace=TRUE,
                    graph=TRUE,col=friendlycolor(14))
boot.medias <- samples[[2]]
boot.sds <- samples[[3]]
legend("right", 
       c("Amostra unica", 
         "Media am. +-3 d.p.",
         paste("bootstraps: ",B,sep=""),
         "Media boot +-3 d.p."
       ), 
       col=c(friendlycolor(3),
             paste(friendlycolor(3),"88",sep=""),
             friendlycolor(14),
             paste(friendlycolor(14),"88",sep="")             
       ),
       lwd=c(2,10,2,10), 
       lty=c(1,1,1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  
# distribuicao da amostra original, sobreposta para referencia
plot.density.singleline(amo.unique,col=friendlycolor(3))

um <- mean(amo.unique,na.rm=TRUE)
us <- mean(amo.unique,na.rm=TRUE)
bm <- mean(boot.medias,na.rm=TRUE)
bs <- mean(boot.sds,na.rm=TRUE)
ep <- sd(boot.medias,na.rm=TRUE)
v <- ""
v <- paste(v,"Amostra:\n")
v <- paste(v,"\tmedia amostral:",round(um,4),"\n")
v <- paste(v,"\td.p. amostral:",round(us,4),"\n")
v <- paste(v,"\n")
v <- paste(v,"Reamostras:",B,"com n =",n,"\n")
v <- paste(v,"\tmedia das medias das reamostras:",round(bm,4),"\n")
v <- paste(v,"\tmedia dos d.p. da media das reamostras:",round(bs,4),"\n")
v <- paste(v,"\terro padrão (d.p. das medias das reamostras:",round(ep,4))
cat(v)

# distribuicao das medias das reamostras (deixando a escala livre)
plot.density.withmeansd(boot.medias,
                        main="Distribuicao de medias das reamostras\n(bootstrapping)",
                        xlab="Valores",
                        ylab="Densidade de probabilidades",
                        col=friendlycolor(14))
# ajustando distribuicao normal
um <- mean(amo.unique,na.rm=TRUE)
us <- mean(amo.unique,na.rm=TRUE)
bm <- mean(boot.medias,na.rm=TRUE)
ep <- sd(boot.medias,na.rm=TRUE)
x <- seq(bm-4*ep,bm+4*ep,length.out=100)
y <- dnorm(x, mean=bm, sd=ep)
lines(x,y,lty=2,lwd=2,col=friendlycolor(3))
# intervalo de confianca 95%
icx <- quantile(boot.medias,probs=c((1-0.95)/2,1-((1-0.95)/2)))
icy <- mean(y[which(round(x,0)==round(icx,0))],na.rm=TRUE)
h <- icy/5
lines(c(icx[1],icx[1],icx[1],icx[2],icx[2],icx[2]),
      c(icy-h ,icy+h ,icy   ,icy   ,icy+h ,icy-h ),
      lwd=2, col=friendlycolor(14))
# media das reamostras (ponto solido)
points(bm,icy,pch=21,cex=1.5,col=friendlycolor(14),bg=friendlycolor(14))
# media amostral (ponto solido)
points(um,icy-h,pch=21,cex=1.5,col=friendlycolor(3),bg=friendlycolor(3))
# media populacional (ponto branco)
points(pm,icy-2*h,pch=21,cex=1.5,col=friendlycolor(3),bg="white")
