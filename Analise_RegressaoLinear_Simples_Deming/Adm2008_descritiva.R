# Biometria_rPearson.R
source("eiras.friendlycolor.R")
source("eiras.correg.R")
alfa <- 0.05
B <- 0
col <- friendlycolor(7) # azul
pch <- 24

Dados <- readxl::read_excel("Adm2008.xlsx")

lst <- correg(Dados$Estatura, Dados$MCT,
              method="preview",
              alpha=alfa, B=B, 
              xlab="Estatura (cm)", ylab="Massa (kg)", 
              col=col, bg="transparent", pch=pch)
