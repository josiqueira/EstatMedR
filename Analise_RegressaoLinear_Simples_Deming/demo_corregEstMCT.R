source("eiras.friendlycolor.R")
source("eiras.correg.R")

# valores
estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)

# scatterplot e regressao linear simples
lst <- correg(estatura, massa, method="lm",
              xlab="Estatura (cm)", ylab="Massa (kg)",
              pch=21, col="#000000", bg=friendlycolor(24))
