estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
cat("estaturas:",estatura,"\n")
cat("massas corporais:",massa,"\n")
sunflowerplot(estatura, massa, 
              xlab = "Estatura (cm)", ylab="Massa corporal (kg)",
              pch=21, col="black")
lines(lowess(estatura, massa), lty=2)
