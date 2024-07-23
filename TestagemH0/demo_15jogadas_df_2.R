source("eiras.friendlycolor.R")
jogadas <- 15
sucesso <- 0:jogadas
prob.sucesso <- 0.5
probabilidade <- dbinom(sucesso,jogadas,prob.sucesso)
binomial <- data.frame(sucesso,probabilidade)
names(binomial) <- c("Sucesso","FR")
plot(binomial$Sucesso, 
     binomial$FR,
     main = paste("Binomial: ",
                  jogadas, " jogadas", sep=""),
     ylim = c(0,max(binomial$FR)),
     xlab="Sucessos",
     ylab="Probabilidade",
     type="h", 
     col=friendlycolor(8), lwd=3)
points(binomial$Sucesso, 
       binomial$FR,
       pch=21, 
       col=friendlycolor(8), 
       bg=friendlycolor(12))
