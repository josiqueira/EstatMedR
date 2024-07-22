source("eiras.friendlycolor.R")
source("eiras.create.population.R")
source("eiras.plot.density.withmeansd.R")
source("eiras.sampling.R")

set.seed(27)

# create a simulated population
N <- c(2000,6000,3500,3000)
population <- create.population(kind="normal", round=0, 
                                n=N,
                                param1=c(40,80,110,160), # mean
                                param2=c(5,10,30,15) # sd
)
pm <- mean(population)
limits <- plot.density.withmeansd(population,
                                  main=paste0("Populacao simulada\nN = ",sum(N)),
                                  xlab="Colesterol LDL (mg/dl)",
                                  ylab="Densidade",
                                  col=friendlycolor(7), getlimits=TRUE)
n <- 36
plot.density.withmeansd(population,
                        main=paste0("Populacao simulada\ne amostra unica (n = ",n,")"),
                        xlab="Colesterol LDL (mg/dl)",
                        ylab="Densidade",
                        x.min=limits[1], x.max=limits[2],
                        y.min=limits[3], y.max=limits[4],
                        col=paste0(friendlycolor(7),"40"))
# amostra unica
B2 <- 1
samples <- sampling(population,B=B2,
                    n=n,replace=FALSE,
                    y.pos=limits[4]*1.15,
                    graph=TRUE,col=friendlycolor(1))
amo.unique <- samples[[1]]
legend("right", 
       c("Populacao", 
         "Media pop +-3 dp",
         "Amostra",
         "Media am. +-3 dp"
       ), 
       col=c(paste0(friendlycolor(7),"40"),
             paste(friendlycolor(7),"88",sep=""),
             friendlycolor(1),
             paste(friendlycolor(1),"88",sep="")             
       ),
       lwd=c(2,10,2,10), 
       lty=c(1,1,1,1), 
       cex=0.7,
       box.lwd=0, bg="transparent")  

# relato
pm <- mean(population,na.rm=TRUE)
ps <- sd(population,na.rm=TRUE)
am <- mean(amo.unique,na.rm=TRUE)
as <- sd(amo.unique,na.rm=TRUE)
v <- ""
v <- paste(v,"Populacao:\n")
v <- paste(v,"\tmedia populacional:",round(pm,4),"\n")
v <- paste(v,"\td.p. populacional:",round(ps,4),"\n")
v <- paste(v,"\n")
v <- paste(v,"\tmedia amostral:",round(am,4),"\n")
v <- paste(v,"\td.p. amostral:",round(as,4))
cat(v)
