
source("eiras.friendlycolor.R")

# defaults

if (!exists("alfa"))
{
  alfa <- 0.05
}
if(!exists("jogadas"))
{
  jogadas <- 15
}
if(!exists("pH0"))
{
  pH0 <- 0.5
}
if(!exists("pH1"))
{
  pH1 <- 2/3
}

sucesso <- 0:jogadas
H0 <- dbinom(sucesso,jogadas,pH0)
H1 <- dbinom(sucesso,jogadas,pH1)

# H0
plot(sucesso, H0,
     main = paste("Binomial: ",
                  jogadas, " jogadas", 
                  sep=""),
     xlab = "Sucesso",
     ylab = "Probabilidade",
     ylim = c(0,max(H0,H1))*1.2, 
     type="h", 
     col=friendlycolor(8), lwd=3)
points(sucesso, H0, 
       pch=21, 
       col=friendlycolor(8), 
       bg=friendlycolor(12))
# H1
lines(sucesso+0.2, H1,
      type="h", 
      col=friendlycolor(27), lwd=3)
points(sucesso+0.2, H1, 
       pch=21, 
       col=friendlycolor(27), 
       bg=friendlycolor(30))
# cutoff 5%
cutoff <- jogadas+1
while (sum(H0[cutoff:(jogadas+1)]) < alfa)
{
  cutoff <- cutoff-1
}
abline(v=cutoff-0.5,lty=2, lwd=2)
# beta
beta <- sum(dbinom(0:(cutoff-1),jogadas,pH1))
hachura <- 500/jogadas
if (hachura < 10) {hachura <- 10}
if (hachura > 35) {hachura <- 35}
lines (sucesso[1:cutoff], H1[1:cutoff],
       col=paste(friendlycolor(27),"88",sep=""), 
       lwd=hachura, type="h")
# Relatorio
cat("H0: Binomial(n=",jogadas,",p=",pH0,")","\n",sep="")
cat("H1: Binomial(n=",jogadas,",p=",pH1,")","\n",sep="")
cat("\nponto de corte:",cutoff-1," | ",cutoff,"\n")
cat("alfa = ",round(alfa*100,2),"%\n")
cat("beta = ",round(beta*100,2),"%\n")
cat("poder = ",round((1-beta)*100,2),"%\n")
