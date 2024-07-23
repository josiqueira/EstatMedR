layout(matrix(1:5,nrow=1,ncol=5))
source("eiras.friendlycolor.R")
p.sucesso <- 0.5
for (jogadas in c(1,2,3,5,15))
{
  probabilidade <- dbinom(0:jogadas,jogadas,0.5)
  sucesso <- 0:jogadas
  plot(sucesso, probabilidade,
       axes = FALSE,
       main = paste("Bin(",jogadas,", ",p.sucesso,")", sep=""),
       xlab=NA, ylab=NA,
       xlim= c(0,15), ylim = c(0,0.5),
       type="h", col=friendlycolor(8), lwd=3)
  points(sucesso,probabilidade, pch=21, col=friendlycolor(8), 
         bg=friendlycolor(12))
}
par(mfrow=c(1,1))
