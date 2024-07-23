# demo_r.R

source("BiNormal.R")

# s <- round(runif(1,1,10000))
# cat(s,"\n")
# s <- 6597
# set.seed(s)

# cria dados com o valor r pedido e plota 
r <- 0.95
n <- 1e3
alfa <- 0.05
lst <- binormal(correl=r, n=n)
dados <- lst[[2]]
# correlacao
crl <- cor.test(dados$X, dados$Y)
# grafico
plot(dados$X, dados$Y, 
     main = paste("n = ",n,", r = ",round(crl$estimate,3), sep=""),
     xlab="X", ylab="Y",
     xlim=c(-4,4), ylim=c(-4,4),
     pch=21, col="#00000088", bg="#6195CF33")
# bissetriz
lines(c(-4,4),c(-4,4),lty=2)
# exibe a correlacao
print(crl)

# regressao linear y~x dos valores gerados
rls <- lm(dados$Y~dados$X)
x <- seq(min(dados$X),max(dados$X),length.out = 10)
yhat <- rls$coefficients[1] + rls$coefficients[2]*x
lines(x,yhat,lwd=2)
print(summary(rls))

# elipse de confianca 95%
de <- car::dataEllipse(dados$X, dados$Y,
                       levels=1-alfa,
                       draw=FALSE)
lines(de, lwd=2)
abline(v=min(de[,1]),lwd=1,lty=2)
abline(v=max(de[,1]),lwd=1,lty=2)
abline(h=min(de[,2]),lwd=1,lty=2)
abline(h=max(de[,2]),lwd=1,lty=2)


# regressao linear invertida x~y
rls2 <- lm(dados$X~dados$Y)
y <- seq(min(dados$Y),max(dados$Y),length.out = 10)
xhat <- rls2$coefficients[1] + rls2$coefficients[2]*y
lines(xhat,y,lwd=2)
print(summary(rls2))
