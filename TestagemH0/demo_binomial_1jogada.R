suppressMessages(library(ggplot2, warn.conflicts = FALSE))
suppressMessages(library(png, warn.conflicts = FALSE))
suppressMessages(library(grid, warn.conflicts = FALSE))
suppressMessages(library(ggimage, warn.conflicts = FALSE))

layout(matrix(1:2,nrow=1),widths=c(1,2))
# imagem das moedas
plot(NA,NA,xlim=c(0,10),ylim=c(0,10), 
     xlab="", ylab="", axes=FALSE)
img <- readPNG("image/moeda_1jogada.png")
graphics::rasterImage(img,0,0,10.5,5)
# grafico
source("eiras.friendlycolor.R")
jogadas <- 1
probabilidade <- dbinom(0:jogadas,jogadas,0.5)
sucesso <- 0:jogadas
plot(sucesso, probabilidade,
     main = paste("Binomial: ",jogadas, " jogada", sep=""),
     ylim = c(0,0.5),
     type="h", col=friendlycolor(8), lwd=3,
     axes=FALSE)
axis(2)
axis(1, at=c(0,1))
points(sucesso,probabilidade, pch=21, col=friendlycolor(8), 
       bg=friendlycolor(12))
par(mfrow=c(1,1))
