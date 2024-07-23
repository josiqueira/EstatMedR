source("eiras.friendlycolor.R")
jogadas <- 5
sucesso <- 0:jogadas
probabilidade <- dbinom(sucesso,jogadas,0.5)
plot(sucesso, probabilidade,
     main = paste("Binomial: ",
                  jogadas, " jogadas", sep=""),
     ylim = c(0,0.5),
     type="h", 
     col=friendlycolor(8), lwd=3)
points(sucesso,probabilidade, pch=21, 
       col=friendlycolor(8), 
       bg=friendlycolor(12))
