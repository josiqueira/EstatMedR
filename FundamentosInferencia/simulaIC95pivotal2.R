library(DescTools)

# s <- round(runif(1,1,10000))
# print (s)
set.seed(4027) 

N <- 1e6
alfa <- 0.05 # nivel de significancia

# populacao sem distribuicao normal
pop <- create.population(kind="normal", round=0, 
                         n=c(8000),
                         param1=c(171), # mean
                         param2=c(7) # sd
)

cat ("Média populacional = ",round(mean(pop),0),"\n",sep="")
cat ("Desvio padrão populacional = ",round(sd(pop),0),"\n",sep="")

# amostra unica
n <- 30 # tamanho da amostra
# v <- rnorm(n, mp, dpp) # amostra aleatoria normal de tamanho n
v <- sample(pop, size=n, replace=FALSE) # amostra aleatoria normal de tamanho n

# exibe populacao e amostra
d.pop <- density(pop)
d.v <- density(v)
plot(d.pop, 
     main=NA,
     xlab="Estatura (cm)", ylab="Densidade",
     xlim=c(min(d.pop$x,d.v$x),max(d.pop$x,d.v$x)),
     ylim=c(min(d.pop$y,d.v$y),max(d.pop$y,d.v$y)*1.2),
     lwd=4, lty=1, col="#1965B010")
lines(d.v, lty=1, lwd=2, col="#BA8DB4")

m <- mean(v) # media amostral
s <- sd(v) # desvio-padrao amostral
a <- DescTools::Skew(v) # skewness
ep <- s/sqrt(n) # erro-padrao
cat ("Média amostral obtida=",round(m,3),"\n",sep="")
cat ("Desvio padrão amostral obtido=",round(s,3),"\n",sep="")
cat ("Assimetria=",round(a,3),"\n",sep="")

# bootstrapping pivotal
B <- 1e5
Tstar <- rep(0,B)
for (i in 1:B)
{
  x <- sample(v, n, replace=TRUE)
  Tstar[i] <- (mean(x)-m)/(sd(x)/sqrt(n))
}
ICpv <- m + quantile(Tstar,c(alfa/2,1-alfa/2))*ep

# relatorio
cat("\n")
cat("ICBootPivotal",round((1-alfa)*100,0),"(media pop):\n", sep="")
cat(ICpv," (em relação à média:",ICpv-m,")\n")
cat("\n")
cat("IC95 da media populacional: bootstrapping por percentil\n")
q <- quantile(replicate(B, mean(sample(v, replace=TRUE))), 
              probs=c(alfa/2,1-alfa/2))
cat(q," (em relação à média:",q-m,")\n")
cat("\n")
cat("IC95 da media populacional: teste t para um grupo:\n")
t <- t.test(v, mu=mp)$conf.int
cat(t," (em relação à média:",t-m,")\n")
cat("(esta é a estimativa tradicional paramétrica, centrada na média)")

# intervalos no grafico
y <- max(d.pop$y,d.v$y)*1.2
lines(ICpv,rep(y,2),lty=1)
points(ICpv,rep(y,2),pch="I")
points(m,y,pch=21,col="black",bg="black",cex=0.6)
text(ICpv[2],y,"IC95 pivotal",pos=4,cex=0.8)

y <- max(d.pop$y,d.v$y)*1.15
lines(q,rep(y,2),lty=1)
points(q,rep(y,2),pch="I")
points(m,y,pch=21,col="black",bg="black",cex=0.6)
text(q[2],y,"IC95 percentílico",pos=4,cex=0.8)

y <- max(d.pop$y,d.v$y)*1.10
lines(t,rep(y,2),lty=1)
points(t,rep(y,2),pch="I")
points(m,y,pch=21,col="black",bg="black",cex=0.6)
text(t[2],y,"IC95 (teste t)",pos=4,cex=0.8)

# media populacional
abline(v=mean(pop), lty=2, col="#222222")