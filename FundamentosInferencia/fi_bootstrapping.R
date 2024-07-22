source("eiras.plot.density.withmeansd.R")

# bootstrapping 
B <- 10000 # aumente o numero de reamostras para melhor resultado
plot.density.withmeansd(amo.unique,
                        main=paste("Amostra unica (n=",n,")\n(bootstrapping com ",B," reamostragens)",sep=""),
                        xlab="Colesterol LDL (mg/dl)",
                        ylab="Densidade",
                        x.min=limits[1], x.max=limits[2],
                        y.min=limits[3], y.max=limits[4],
                        col=friendlycolor(3))
samples <- sampling(amo.unique,B=B,
                    n=n,replace=TRUE,
                    graph=TRUE,col=friendlycolor(29))
boot.medias <- samples[[2]]
boot.sds <- samples[[3]]
plot.showmeansd(mean(boot.medias), 
                mean(boot.sds), 
                color=friendlycolor(29),
                y.pos=limits[4]*1.15)  
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
plot.density.singleline(amo.unique,col=friendlycolor(3))
# relato
am <- mean(amo.unique,na.rm=TRUE)
as <- sd(amo.unique,na.rm=TRUE)
bm <- mean(boot.medias)
bs <- mean(boot.sds)
epm <- sd(boot.medias)
ic95m <- quantile(boot.medias,probs=c(0.05/2,1-0.05/2))
eps <- sd(boot.sds)
ic95s <- quantile(boot.sds,probs=c(0.05/2,1-0.05/2))

# distribuicao das medias amostrais (mantendo a escala)
plot.density.withmeansd(boot.medias,
                        main="Distribuicao de medias amostrais\n(bootstrapping)",
                        xlab="Medias amostrais de Colesterol LDL (mg/dl)",
                        ylab="Densidade de probabilidades",
                        x.min=limits[1], x.max=limits[2],
                        col=friendlycolor(29))
# relato
v <- ""
v <- paste0(v,"Amostra unica (n = ",n,"):\n")
v <- paste0(v,"\tmedia amostral: ",round(am,4),"\n")
v <- paste0(v,"\td.p. amostral: ",round(as,4))
v <- paste0(v,"\nAmostras:",B," com n = ",n,"\n")
v <- paste0(v,"\tmedia das medias amostrais: ",round(bm,4),"\n")
v <- paste0(v,"\tmedia dos d.p. amostrais: ",round(bs,4),"\n")
v <- paste0(v,"\n")
v <- paste0(v,"\terro padrao da media: ",round(epm,4),"\n")
v <- paste0(v,"\tIC95(media): [",round(ic95m[1],4),",",round(ic95m[2],4),"]","\n")
cat(v)
