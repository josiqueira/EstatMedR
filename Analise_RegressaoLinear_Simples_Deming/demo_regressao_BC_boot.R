# demo_regressao_BC_boot.R

source("eiras.friendlycolor.R")
source("eiras.ConfidenceBand.R")

# valores
estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)

# scatterplot
plot(estatura, massa,
     xlim = c(170,182), ylim = c(55,90),
     xlab="Estatura (cm)", ylab="Massa (kg)",
     pch=21, col="black", bg=friendlycolor(24)
     )

# modelo linear, traca a reta
modelo <- lm(massa ~ estatura)
intercepto <- modelo$coefficients[1]
inclinacao <- modelo$coefficients[2]
massa.medio <- intercepto + inclinacao*estatura
lines(estatura, massa.medio, col=friendlycolor(8), 
      lwd=3, lty=1)

# media de estatura e media de massa
centro_estatura <- mean(estatura)
centro_massa <- mean(massa)
# media de massa, traca linha horizontal
massa.H0 <- rep(centro_massa,length(estatura))
lines(estatura, massa.H0, col=friendlycolor(30), 
      lwd=3, lty=2)
# centroide, plota o ponto
points(centro_estatura, centro_massa, pch=21, col="black", bg="black")
# banda de confianca
alfa <- 0.05
B <- 1e3
lst <- ConfidenceBand(x=estatura, y=massa, alpha=alfa, B=B)
band <- lst[[2]]
lines(band$X, band$LB, col=friendlycolor(8), lwd=2, lty=2)
lines(band$X, band$UB, col=friendlycolor(8), lwd=2, lty=2)

