source("eiras.friendlycolor.R")
source("eiras.cor.test.boot.R")
source("eiras.correg.R")

col_HB <- friendlycolor(30) # tijolo
pch_HB <- 22
col_HEM <- friendlycolor(28) # bordo
pch_HEM <- 23
col_LEUC <- friendlycolor(9) # azul
pch_LEUC <- 24
B <- 1e4
# HT x HB
correg(Gestantes$HT, Gestantes$HB, method="pearson", B=B,
              col=col_HB, bg=paste(col_HB,"50",sep=""), pch=pch_HB,
              xlab="Hematocrito (%)", ylab="Hemoglobina (mg/dl)")

# HT x HEM
correg(Gestantes$HT, Gestantes$HEM, method="pearson", B=B,
       col=col_HEM, bg=paste(col_HEM,"50",sep=""), pch=pch_HEM,
       xlab="Hematocrito (%)", ylab="Hemacias (milhoes/mm3)")

# HT x LEUC
correg(Gestantes$HT, Gestantes$LEUC, method="pearson", B=B,
       col=col_LEUC, bg=paste(col_LEUC,"50",sep=""), pch=pch_LEUC,
       xlab="Hematocrito (%)", ylab="Leucocitos (milhares/mm3)")
