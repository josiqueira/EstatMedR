# demo_regressao_3.R
# alavancagem produzida por mudança de coordenadas
# do ponto (x3,y3)

source("eiras.friendlycolor.R")

# valores
x <- estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
y <- massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
# para nao ter um ponto na coordenada do centroide
# (melhora o exemplo)
centro_x <- mean(x)
x[x==centro_x] <- x[x==centro_x]-0.5
# cat("x:",x,"\n")
# cat("y:",y,"\n")

# centroide (media de x, media de y)
centro_x <- mean(x)
centro_y <- mean(y)
# para nao ter um ponto na coordenada do centroide
x[x==centro_x] <- x[x==centro_x]-0.5
centro_x <- mean(x)

# scatterplot
plot(NA, 
     xlab="Estatura (cm)", ylab="Massa (kg)",
     xlim = c(min(x),max(x)), ylim = c(min(y),max(y)),
     pch=21, col="black", bg=friendlycolor(24)
     )

# media de y, traca linha horizontal
y.H0 <- rep(centro_y,length(x))
lines (x, y.H0, col=friendlycolor(30), 
       lwd=3, lty=2)

# modelo linear, traca a reta
modelo <- lm(y ~ x)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
y.medio <- intercepto + inclinacao*x
lines (x, y.medio, 
       col=paste(friendlycolor(8),"44",sep=""), 
       lwd=3, lty=1
       )
points(x, y,
     pch=21, col="darkgray", 
     bg=paste(friendlycolor(24),"44",sep="")
     )

# centroide
points(centro_x, centro_y, pch=21, col="black", bg="black")

# modelo linear, traca a reta
original.x <- x[3]
original.y <- y[3]
x[3] <- 180
y[3] <- 63
modelo <- lm(y ~ x)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
y.medio <- intercepto + inclinacao*x
lines (x, y.medio, 
       col=friendlycolor(8), 
       lwd=3, lty=1
)
points(x, y,
       pch=21, col="black", 
       bg=friendlycolor(24)
)
arrows(original.x, original.y, x[3], y[3], length = 0.2, lty=2)

# legenda
legend ("topleft",
        c("y observado","média de y",
          paste("reta: y_médio = ",round(intercepto,3)," + ",
                round(inclinacao,3),"x",sep="")
        ), 
        lwd=c(NA, 3, 3), 
        pch=c(21, NA, NA),
        col=c("black",friendlycolor(30), friendlycolor(8)), 
        pt.bg=c(friendlycolor(24),friendlycolor(30), friendlycolor(8)), 
        cex=0.9, bty="n")

