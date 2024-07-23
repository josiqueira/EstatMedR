# demo_regressao.R

source("eiras.friendlycolor.R")
source("eiras.ConfidenceBand.R")

# valores
estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
# centrando x
estatura <- estatura-mean(estatura)
cat("estatura(centrada): ",estatura,"\n")
cat("massa(original): ",massa,"\n")

# scatterplot
plot(estatura, massa,
     xlim = c(min(estatura),max(estatura)), 
     ylim = c(55,95),
     xlab="Estatura centrada (cm)", ylab="Massa (kg)",
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

# parte explicada pelo modelo
for (i in 1:length(estatura))
{
  lines(c(estatura[i]-0.05,estatura[i]-0.05), c(massa.medio[i],massa.H0[i]), 
        lwd=2, lty=3, col=friendlycolor(8))
}

# resíduo
for (i in 1:length(estatura))
{
  lines(c(estatura[i]+0.05,estatura[i]+0.05), c(massa[i],massa.medio[i]), 
        lwd=2, lty=4, col=friendlycolor(25))
}


# legenda
legend("topleft",
        c("valores observados","média de massa",
          paste("reta: massa_media = ",round(intercepto,3)," + ",
                round(inclinacao,3)," x estatura",sep=""),
          "parte explicada",
          "resíduo"
        ), 
        lty=c(NA, 1, 1, 3, 4), 
        lwd=c(NA, 3, 3, 2, 2), 
        pch=c(21, NA, NA, NA, NA),
        col=c("black",friendlycolor(30), friendlycolor(8), 
              friendlycolor(8), friendlycolor(25)), 
        pt.bg=c(friendlycolor(24),friendlycolor(30), friendlycolor(8), 
                friendlycolor(8), friendlycolor(25)), 
        cex=0.9, bty="n")

sumario <- summary(modelo)
print(sumario)
