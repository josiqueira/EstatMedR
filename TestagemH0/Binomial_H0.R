source("eiras.friendlycolor.R")

if(!exists("cauda"))
{
  cauda <- 0 # a partir de onde hachurar
}

# largura da hachura
hachura <- 500/jogadas
if (hachura < 10) {hachura <- 10}
if (hachura > 35) {hachura <- 35}
plot(binomial$Sucesso, 
     binomial$FR,
     main = paste("Binomial: ",
                  jogadas, " jogadas, ", 
                  "P[sucesso] = ", p.sucesso, 
                  sep=""),
     xlab = "Sucesso",
     ylab = "Probabilidade",
     ylim = c(0,max(probabilidade)*1.2), 
     type="h", 
     col=friendlycolor(8), lwd=3)
points(sucesso,probabilidade, pch=21, 
       col=friendlycolor(8), 
       bg=friendlycolor(12))
# cauda direita (alfa)
lines (binomial$Sucesso[binomial$Sucesso>=12], 
       binomial$FR[binomial$Sucesso>=12],
       col=paste(friendlycolor(8),"88",sep=""), 
       lwd=hachura, type="h")
# cauda direita (p)
lines (binomial$Sucesso[binomial$Sucesso>=10], 
       binomial$FR[binomial$Sucesso>=10],
       col=paste(friendlycolor(27),"88",sep=""), 
       lwd=hachura/2, type="h")
# areas e labels das áreas
y <- max(binomial$FR[binomial$Sucesso<cauda])*3
x <- 15-cauda+0.5
# polygon(x=c(jogadas+1,x,x,jogadas+1,jogadas+1), 
#         y=c(-1,-1,y,y,-1), 
#         col=paste(friendlycolor(30),"88",sep=""),
#         bg=paste(friendlycolor(30),"88",sep=""))
abline(v=x,lty=2)
text(max(binomial$Sucesso)*1/2,
     max(binomial$FR)*1.1,
     "não rejeição\nde H0")
# x <- mean(binomial$Sucesso[binomial$Sucesso<cauda])
# y <- max(binomial$FR[binomial$Sucesso<cauda])*5
# text(x, y, "rejeição\nde H0")
x <- mean(binomial$Sucesso[binomial$Sucesso>jogadas-cauda])
y <- (max(binomial$FR)*1.1)/2
text(x, y, "rejeição\nde H0")
legend ("topright", c("alfa","p"), 
        lwd=hachura*0.7, 
        col=c(paste(friendlycolor(8),"88",sep=""),
              paste(friendlycolor(27),"88",sep="")
        ), 
        box.lwd=0)
