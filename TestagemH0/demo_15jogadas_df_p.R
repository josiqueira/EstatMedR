source("eiras.friendlycolor.R")

if (!exists("color"))
{
        color<-8
}
if (!exists("background"))
{
        background<-12
}
if (!exists("prob.sucesso"))
{
        prob.sucesso <- 0.5
}

jogadas <- 15
sucesso <- 0:jogadas
probabilidade <- dbinom(sucesso,jogadas,prob.sucesso)
binomial <- data.frame(sucesso,probabilidade)
names(binomial) <- c("Sucesso","FR")
plot(binomial$Sucesso, 
     binomial$FR,
     main = paste("Binomial",
                  " (n=",jogadas,", p=",prob.sucesso,")",
                  sep=""),
     ylim = c(0,max(binomial$FR)),
     xlab="Sucessos",
     ylab="Probabilidade",
     type="h", 
     col=friendlycolor(color), lwd=3)
points(binomial$Sucesso, 
       binomial$FR,
       pch=21, 
       col=friendlycolor(color), 
       bg=friendlycolor(background))
