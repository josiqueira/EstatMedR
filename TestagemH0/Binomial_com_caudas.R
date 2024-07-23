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
     ylim = c(0,max(probabilidade)), 
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
