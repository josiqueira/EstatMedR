source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")

col_HB <- friendlycolor(30) # tijolo
pch_HB <- 22
col_HEM <- friendlycolor(28) # bordo
pch_HEM <- 23
col_LEUC <- friendlycolor(9) # azul
pch_LEUC <- 24

Gestantes <- readRDS("Gestante.rds")

# HT x HB
with(Gestantes,correg(HT, HB, method="pearson",
                      xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)",
                      col=col_HB, bg="transparent", pch=pch_HB))

# HT x HEM
with(Gestantes,correg(HT, HEM, method="pearson",
                      xlab="Hematocrito (%)", ylab="Hemacias (milhoes/mm3)",
                      col=col_HEM, bg="transparent", pch=pch_HEM))

# HT x LEUC
with(Gestantes,correg(HT, LEUC, method="pearson",
                      xlab="Hematocrito (%)", ylab="Leucocitos (milhares/mm3)",
                      col=col_LEUC, bg="transparent", pch=pch_LEUC))
