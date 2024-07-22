library(DescTools)

source("eiras.friendlycolor.R")

# s <- round(runif(1,1,10000))
# print (s)
set.seed(4485)

N <- 1e6
alfa <- 0.05 # nivel de significancia

# Este trecho foi usado para gerar a planilha (dados ficticios)
# mp <- 66 # media populacional teorica
# dpp <- 6 # desvio-padrao populacional teorico
# popA <- rnorm(N, mean=mp, sd=dpp)
# n <- 30 # tamanho da amostra
# vA <- sample(popA, size=n, replace=FALSE)
# # peso diminuido apos a dieta
# vB <- vA - runif(n,min=-1,max=2)
# dt_dieta <- data.frame(round(vA,1),round(vB,1))
# names(dt_dieta) <- c("Antes","Depois")
# dt_dieta$Dif <- (dt_dieta$Depois-dt_dieta$Antes)*1000
# openxlsx::write.xlsx(dt_dieta,"Dieta.xlsx")

# dados
dt_dieta <- readxl::read_excel("Dieta.xlsx")

# exibe populacoes e amostras
vA <- dt_dieta$Antes
vB <- dt_dieta$Depois
n <- nrow(dt_dieta)
corA <- friendlycolor(25)
corB <- friendlycolor(13)
d.vA <- density(vA)
d.vB <- density(vB)
xmin <- min(d.vA$x,d.vB$x)
xmax <- max(d.vA$x,d.vB$x)
ymin <- min(d.vA$y,d.vB$y)
ymax <- max(d.vA$y,d.vB$y)
plot(NA, 
     main="Massa corporal total de mulheres sob dieta",
     xlab="MCTotal (kg)", ylab="Densidade",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax))
lines(d.vA, lty=1, lwd=2, col=corA)
lines(d.vB, lty=1, lwd=2, col=corB)
# medias amostrais
abline(v=mean(vA), lty=2, col=corA)
abline(v=mean(vB), lty=2, col=corB)
legend("topright", 
       c("Antes", "Depois"), 
       col=c(corA, corB),             
       lwd=2, 
       lty=1, 
       cex=0.8,
       box.lwd=0, 
       bg="transparent")  

mA <- mean(vA) # media amostral
sA <- sd(vA) # desvio-padrao amostral
aA <- DescTools::Skew(vA) # skewness
mB <- mean(vB) # media amostral
sB <- sd(vB) # desvio-padrao amostral
aB <- DescTools::Skew(vB) # skewness


cat ("n=",n,"\n\n",sep="")

cat ("Antes\n")
cat ("Média amostral obtida=",round(mA,3),"kg\n",sep="")
cat ("Desvio padrão amostral obtido=",round(sA,3),"kg\n",sep="")
cat ("Assimetria=",round(aA,3),"\n",sep="")

cat ("Depois\n")
cat ("Média amostral obtida=",round(mB,3),"kg\n",sep="")
cat ("Desvio padrão amostral obtido=",round(sB,3),"kg\n",sep="")
cat ("Assimetria=",round(aB,3),"\n",sep="")

# diferenca depois - antes
# bootstrapping
B <- 1e5
Tstar <- rep(0,B)

vdif <- dt_dieta$Dif
mdif <- mean(vdif)
dpdif <- sd(vdif)
epdif <- dpdif/sqrt(n) # erro padrao

means <- c()
for (i in 1:B)
{
  x <- sample(vdif, n, replace=TRUE)
  mx <- mean(x)
  Tstar[i] <- (mx-mdif)/(sd(x)/sqrt(n))
  means <- c(means,mx)
}
ICpv <- mdif + quantile(Tstar,c(alfa/2,1-alfa/2))*epdif
ICpc <- quantile(means,probs=c(alfa/2,1-alfa/2))

# relatorio
cat("\n")
cat("ICBootPercentilico",round((1-alfa)*100,0),"(diferença pop):\n", sep="")
cat(ICpc," (em relação à diferença média:",ICpc-mdif,")\n")
cat("\n")
cat("ICBootPivotal",round((1-alfa)*100,0),"(diferença pop):\n", sep="")
cat(ICpv," (em relação à diferença média:",ICpv-mdif,")\n")
cat("\n")
cat("IC95 da media populacional: teste t para um grupo:\n")
t <- t.test(vdif, mu=0)$conf.int
cat(t," (em relação à diferença média:",t-mdif,")\n")
cat("(esta é a estimativa tradicional paramétrica, centrada na média)")


cordif <- friendlycolor(2)
d.vdif <- density(vdif)
ymaxdif <- max(d.vdif$y, na.rm=TRUE)
plot(d.vdif, 
     main="Distribuicao da diferenca de MCT",
     xlab="MCT (Depois-Antes, gramas)", ylab="Densidade",
     ylim=c(0,ymaxdif*1.25),
     col=cordif, lwd=2)
# H0
abline(v=0, lty=2, col="black")
# media da diferenca
abline(v=mdif, lty=2, col=cordif)
text(mdif,0,paste0(round(mdif,0),"g"),cex=0.5)

# intervalos no grafico
y <- ymaxdif*1.20
lines(ICpc,rep(y,2),lty=1)
points(ICpc,rep(y,2),pch="I")
points(mdif,y,pch=21,col="black",bg="black",cex=0.6)
text(ICpc[2],y,"IC95 percentilico",pos=4,cex=0.8)

y <- ymaxdif*1.15
lines(ICpv,rep(y,2),lty=1)
points(ICpv,rep(y,2),pch="I")
points(mdif,y,pch=21,col="black",bg="black",cex=0.6)
text(ICpv[2],y,"IC95 pivotal",pos=4,cex=0.8)

y <- ymaxdif*1.10
lines(t,rep(y,2),lty=1)
points(t,rep(y,2),pch="I")
points(mdif,y,pch=21,col="black",bg="black",cex=0.6)
text(t[2],y,"IC95 (teste t)",pos=4,cex=0.8)

