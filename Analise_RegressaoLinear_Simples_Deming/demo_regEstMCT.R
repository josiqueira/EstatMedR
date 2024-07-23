source("eiras.friendlycolor.R")

# valores
estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)

# scatterplot
plot(estatura, massa,
     xlim = c(170,182), ylim = c(55,90),
     xlab="Estatura (cm)", ylab="Massa (kg)",
     pch=21, col="black", bg=friendlycolor(24)
)
lines(lowess(estatura,massa), lty=2)

# modelo linear, plota a reta
modelo <- lm(massa ~ estatura)
print(modelo)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
massa.medio <- intercepto + inclinacao*estatura
lines (estatura, massa.medio, col=friendlycolor(8), 
       lwd=3, lty=1)
sumario <- summary(modelo)
print(sumario)
