library(DescTools)

source("eiras.friendlycolor.R")

# s <- round(runif(1,1,10000))
# print (s)
set.seed(8628) 

N <- 1e6
alfa <- 0.05 # nivel de significancia

# populacao masculina com distribuicao normal
mp <- 175 # media populacional teorica
dpp <- 8 # desvio-padrao populacional teorico
popM <- rnorm(N, mean=mp, sd=dpp)
cat ("Masculino\n")
cat ("Média populacional=",round(mean(popM),0),"\n",sep="")
cat ("Desvio padrão populacional=",round(sd(popM),0),"\n",sep="")
# populacao feminina com distribuicao normal
mp <- 168 # media populacional teorica
dpp <- 6 # desvio-padrao populacional teorico
popF <- rnorm(N, mean=mp, sd=dpp)
cat ("Feminino\n")
cat ("Média populacional=",round(mean(popF),0),"\n",sep="")
cat ("Desvio padrão populacional=",round(sd(popF),0),"\n",sep="")

# amostra unica
nM <- 30 # tamanho da amostra masculino
nF <- 30 # tamanho da amostra feminino

# amostras aleatorias de tamanho nX
vM <- sample(popM, size=nM, replace=FALSE) 
vF <- sample(popF, size=nF, replace=FALSE) 

# exibe populacoes e amostras
corM <- friendlycolor(8)
corF <- friendlycolor(20)
d.popM <- density(popM)
d.popF <- density(popF)
d.vM <- density(vM)
d.vF <- density(vF)
xmin <- min(d.popM$x,d.vM$x,d.popF$x,d.vF$x)
xmax <- max(d.popM$x,d.vM$x,d.popF$x,d.vF$x)
ymin <- min(d.popM$y,d.vM$y,d.popF$y,d.vF$y)
ymax <- max(d.popM$y,d.vM$y,d.popF$y,d.vF$y)
plot(NA, 
     main=NA,
     xlab="Estatura (cm)", ylab="Densidade",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax))
lines(d.popM, lwd=4, lty=1, col=paste(corM,"44",sep=""))
lines(d.popF, lwd=4, lty=1, col=paste(corF,"44",sep=""))
lines(d.vM, lty=1, lwd=2, col=corM)
lines(d.vF, lty=1, lwd=2, col=corF)
# medias populacionais
abline(v=mean(popM), lty=2, col=corM)
abline(v=mean(popF), lty=2, col=corF)
legend("topright", 
       c("Masculino", "Feminino"), 
       col=c(corM, corF),             
       lwd=2, 
       lty=1, 
       cex=0.8,
       box.lwd=0, 
       bg="transparent")  

mM <- mean(vM) # media amostral
sM <- sd(vM) # desvio-padrao amostral
aM <- DescTools::Skew(vM) # skewness
epM <- sM/sqrt(nM) # erro-padrao
cat ("Masculino (n=",nM,")\n",sep="")
cat ("Média amostral obtida=",round(mM,3),"\n",sep="")
cat ("Desvio padrão amostral obtido=",round(sM,3),"\n",sep="")
cat ("Assimetria=",round(aM,3),"\n",sep="")

mF <- mean(vF) # media amostral
sF <- sd(vF) # desvio-padrao amostral
aF <- DescTools::Skew(vF) # skewness
epF <- sF/sqrt(nF) # erro-padrao
cat ("Feminino (n=",nF,")\n",sep="")
cat ("Média amostral obtida=",round(mF,3),"\n",sep="")
cat ("Desvio padrão amostral obtido=",round(sF,3),"\n",sep="")
cat ("Assimetria=",round(aF,3),"\n",sep="")

# medias amostrais
abline(v=mM, lty=3, col=corM)
abline(v=mF, lty=3, col=corF)

# bootstrapping
difindep <- mM-mF
epindep <- sqrt(epM^2 + epF^2) # erro-padrao da diferenca
B <- 1e5
Tstar <- rep(0,B)
difs <- c()
for (i in 1:B)
{
  xM <- sample(vM, nM, replace=TRUE)
  xF <- sample(vF, nF, replace=TRUE)
  Tstar[i] <- (mean(xM) - mean(xF) - difindep) /
    sqrt(var(xM)/nM + var(xF)/nF)  
  difs <- c(difs,mean(xM)-mean(xF))
}
ICpv <- difindep + quantile(Tstar,c(alfa/2,1-alfa/2))*epindep

# bootstrapping percentilico
ICpc <- quantile(difs,c(alfa/2,1-alfa/2))

# relatorio
cat("\n")
cat("ICBootPercentilico",round((1-alfa)*100,0),"(dif. medias pop):\n", sep="")
cat(ICpc," (em relação à diferença das médias:",ICpc-difindep,")\n")
cat("\n")
cat("ICBootPivotal",round((1-alfa)*100,0),"(dif. medias pop):\n", sep="")
cat(ICpv," (em relação à diferença das médias:",ICpv-difindep,")\n")
cat("\n")
cat("IC95 da dif. das medias populacionais\n(teste t para condições independentes):\n")
t <- t.test(vM,vF)$conf.int
cat(t," (em relação à diferença das médias:",t-difindep,")\n")
cat("(esta é a estimativa tradicional paramétrica, centrada na média)")

# grafico da diferenca das medias reamostrais
mdif <- mean(difs)
densdifs <- density(difs)
ymaxdif <- max(densdifs$y)
plot(densdifs, 
     main=paste0("Diferencas entre as medias das reamostras\n",
                 "nM = ",nM,", nF = ",nF,", B = ",B),
     xlab="Diferenca de Estatura (Masculino-Feminino, cm)", ylab="Densidade",
     ylim=c(0,ymaxdif*1.25),
     col = friendlycolor(2), lwd=4)
abline(v=0, lty=2)
# intervalos 
# percentilico
y <- ymaxdif*1.20
lines(ICpc,rep(y,2),lty=1)
points(ICpc,rep(y,2),pch="I")
points(mdif,y,pch=21,col="black",bg="black",cex=0.6)
text(ICpv[2],y,"IC95 percentilico",pos=4,cex=0.8)
# pivotal
y <- ymaxdif*1.15
lines(ICpv,rep(y,2),lty=1)
points(ICpv,rep(y,2),pch="I")
points(mdif,y,pch=21,col="black",bg="black",cex=0.6)
text(ICpv[2],y,"IC95 pivotal",pos=4,cex=0.8)
# t
y <- ymaxdif*1.10
lines(t,rep(y,2),lty=1)
points(t,rep(y,2),pch="I")
points(mdif,y,pch=21,col="black",bg="black",cex=0.6)
text(t[2],y,"IC95 teste t",pos=4,cex=0.8)
