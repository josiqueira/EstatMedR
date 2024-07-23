source("eiras.friendlycolor.R")

if(!exists("cauda"))
{
  cauda <- 0 # a partir de onde hachurar
}

p.sucesso <- 0.5 # *** probabilidade de sucesso ***
jogadas <- 15
sucesso <- 0:jogadas
probabilidade <- dbinom(sucesso,jogadas,p.sucesso)
# criando um data frame
binomial <- data.frame(sucesso,probabilidade)
names(binomial) <- c("Sucesso", "FR")
# grafico
# largura da hachura
hachura <- 500/jogadas
if (hachura < 10) {hachura <- 10}
if (hachura > 35) {hachura <- 35}
plot(binomial$Sucesso, 
     binomial$FR, # *** usando o dataframe
     main = paste("Binomial: ",
                  jogadas, " jogadas, ", 
                  "P[sucesso] = ", p.sucesso, 
                  sep=""),
     xlab = "Sucesso", # label do eixo x
     ylab = "Probabilidade", # label do eixo y
     ylim = c(0,max(probabilidade)*1.2), 
     type="h", 
     col=friendlycolor(8), lwd=3)
points(sucesso,probabilidade, pch=21, 
       col=friendlycolor(8), 
       bg=friendlycolor(12))
# cauda esquerda
lines (binomial$Sucesso[binomial$Sucesso<cauda], 
       binomial$FR[binomial$Sucesso<cauda],
       col=paste(friendlycolor(8),"88",sep=""), 
       lwd=hachura, type="h")
# cauda direita
lines (binomial$Sucesso[binomial$Sucesso>15-cauda], 
       binomial$FR[binomial$Sucesso>15-cauda],
       col=paste(friendlycolor(8),"88",sep=""), 
       lwd=hachura, type="h")

# parte 2
# areas e labels das áreas
y <- max(binomial$FR[binomial$Sucesso<cauda])*3
x <- cauda-0.5
polygon(x=c(-1,x,x,-1,-1), 
        y=c(-1,-1,y,y,-1), 
        col=paste(friendlycolor(30),"88",sep=""),
        bg=paste(friendlycolor(30),"88",sep=""))
abline(v=x,lty=2)
x <- 15-cauda+0.5
polygon(x=c(jogadas+1,x,x,jogadas+1,jogadas+1), 
        y=c(-1,-1,y,y,-1), 
        col=paste(friendlycolor(30),"88",sep=""),
        bg=paste(friendlycolor(30),"88",sep=""))
abline(v=x,lty=2)
text(max(binomial$Sucesso)*1/2,
     max(binomial$FR)*1.1,
     "não rejeição\nde H0")
x <- mean(binomial$Sucesso[binomial$Sucesso<cauda])
y <- max(binomial$FR[binomial$Sucesso<cauda])*5
text(x, y, "rejeição\nde H0")
x <- mean(binomial$Sucesso[binomial$Sucesso>jogadas-cauda])
y <- max(binomial$FR[binomial$Sucesso>jogadas-cauda])*5
text(x, y, "rejeição\nde H0")
