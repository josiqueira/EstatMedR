# Com bootstrapping
ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
B <- 5000

source("eiras.pseudomediana.R")

pm1 <- pseudomediana(ComTreino,conf.level=0.95,B=B) 
dens_pm1 <- pm1[["density"]]
q1 <- pm1[["statistics"]][3:4]
pm2 <- pseudomediana(SemTreino,conf.level=0.95,B=B) 
dens_pm2 <- pm2[["density"]]
q2 <-  pm2[["statistics"]][3:4]

plot(dens_pm1, 
     main=paste0("Distribuição das pseudomedianas\npor bootstrapping (B=",B,")"),
     xlab="Pseudomediana", ylab="Densidade",
     xlim=c(1.5,4.5), ylim=c(0,5),type="l")
ypos <- 4/20
i <- ypos/2
lines(c(q1[1],q1[1],q1[1],q1[2],q1[2],q1[2]),
      c(ypos+i,ypos-i,ypos,ypos,ypos-i,ypos+i)
)
points(pm1[["statistics"]][1],ypos,pch=21,col="black",bg="black")
lines(dens_pm2, lty=2, lwd=2, col="#444444")
lines(c(q2[1],q2[1],q2[1],q2[2],q2[2],q2[2]),
      c(ypos+i+2.5*i,ypos-i+2.5*i,ypos+2.5*i,ypos+2.5*i,ypos-i+2.5*i,ypos+i+2.5*i),
      col="#444444"
)
points(pm2[["statistics"]][1],ypos+2.5*i,pch=21,
       col="#444444",bg="#444444")
legend("right",
       c("Com treino", "Sem treino"),
       lty=c(1,2),
       lwd=c(1,2),
       col=c("black","#444444"),
       bty="n")


