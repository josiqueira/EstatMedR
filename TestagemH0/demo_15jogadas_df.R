source("eiras.friendlycolor.R")
jogadas <- 15
sucesso <- 0:jogadas
probabilidade <- dbinom(sucesso,jogadas,0.5)
binomial <- data.frame(sucesso,probabilidade)
names(binomial) <- c("Sucesso","FR")
plot(binomial$Sucesso, 
     binomial$FR,
     main = paste("Binomial: ",
                  jogadas, " jogadas", sep=""),
     ylim = c(0,0.5),
     type="h", 
     col=friendlycolor(8), lwd=3)
points(binomial$Sucesso, 
       binomial$FR,
       pch=21, 
       col=friendlycolor(8), 
       bg=friendlycolor(12))
