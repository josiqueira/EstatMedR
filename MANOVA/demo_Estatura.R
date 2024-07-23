library(readxl)
B <- 1e5
alfa <- 0.05
set.seed(123)
precisao <- 0.01/2; 

Dados <- readxl::read_excel("Adm2008.xlsx")
Dados <- Dados[, 1:4]
Matriz.Masc <- as.matrix(Dados[Dados$Genero=="Masculino", 3:4])
N.Masc <- nrow(Matriz.Masc)
plot(density(Matriz.Masc[,1], na.rm=TRUE),
     main=paste("Estudante masculino ADM-FEAUSP-2008\nN =", N.Masc,
                "\nDesvio-padrao de estatura =",
                round(sd(Matriz.Masc[,1], na.rm=TRUE), 4)),
     xlab="Estatura (m)", ylab="Densidade")
estat.media.boot.Masc <- replicate(B, mean(sample(Matriz.Masc[,1], replace=TRUE)))
quantile(estat.media.boot.Masc, probs=c(alfa/2, 1 - alfa/2))
t.test(Matriz.Masc[,1])$conf.int
plot(density(estat.media.boot.Masc, na.rm=TRUE),
     main=paste("Estudante masculino ADM-FEAUSP-2008\nN =", N.Masc,
                "\nErro-padrao da media amostral =",
                round(sd(estat.media.boot.Masc, na.rm=TRUE), 4)),
     xlab="Media amostral da estatura (m)", ylab="Densidade")
mi <- mean(estat.media.boot.Masc, na.rm=TRUE)
EP <- sd(estat.media.boot.Masc, na.rm=TRUE)
x <- seq(from=mi-5*EP, to=mi+5*EP, by=1e-5)
y <- dnorm(x, mean=mi, sd=EP)
lines(x,y,lwd=2,lty=2)
legend("topright", c("Media amostral","Normal"), lty=1:2, cex=.5)

estat.media.boot.Masc <- replicate(B, median(sample(Matriz.Masc[,1], replace=TRUE)))
estat.media.boot.Masc <- estat.media.boot.Masc+runif(B,-precisao,precisao)
quantile(estat.media.boot.Masc, probs=c(alfa/2, 1 - alfa/2))
wilcox.test(Matriz.Masc[,1], exact=FALSE, conf.int=TRUE)$conf.int
plot(density(estat.media.boot.Masc, na.rm=TRUE),
     main=paste("Estudante masculino ADM-FEAUSP-2008\nN =", N.Masc,
                "\nErro-padrao da mediana amostral =",
                round(sd(estat.media.boot.Masc, na.rm=TRUE), 4)),
     xlab="Mediana amostral da estatura (m)", ylab="Densidade")
mi <- mean(estat.media.boot.Masc, na.rm=TRUE)
EP <- sd(estat.media.boot.Masc, na.rm=TRUE)
x <- seq(from=mi-5*EP, to=mi+5*EP, by=1e-5)
y <- dnorm(x, mean=mi, sd=EP)
lines(x,y,lwd=2,lty=2)
legend("topright", c("Mediana amostral","Normal"), lty=1:2, cex=.5)
