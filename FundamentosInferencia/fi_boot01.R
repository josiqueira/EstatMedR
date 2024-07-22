source("eiras.plot.density.withmeansd.R")
source("eiras.sampling.R")

# amostra
media <- 11.9
dp <- 1.5
set.seed (27) # comente esta linha se quiser variar a amostra
n <- 100
amostra <- round(rnorm(n,media,dp),1)

# bootstrapping 
B <- 10000 # aumente o numero de reamostras para melhor resultado
limits <- plot.density.withmeansd(amostra,
                                main=paste("Amostra unica\n(bootstrapping com ",sprintf("%d",B)," reamostragens)",sep=""),
                                xlab="Hb (g/dl)",
                                ylab="Densidade",
                                col=friendlycolor(1),
                                y.min=0, y.max=y.max,
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

