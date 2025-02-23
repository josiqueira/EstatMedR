# Gestantes_RLS.R
# (versao nao padronizada, para regressão)

alfa <- 0.05

source("eiras.jitter.R")
source("eiras.friendlycolor.R")
source("eiras.correg.R")

col_HB <- friendlycolor(30) # tijolo
pch_HB <- 22
col_HEM <- friendlycolor(28) # bordo
pch_HEM <- 23
col_LEUC <- friendlycolor(9) # azul
pch_LEUC <- 24

Gestantes <- readRDS("Gestante.rds")

# HT x HB
B <- 0
reslmr_hb <- correg(Gestantes$HT, Gestantes$HB,
                    alpha=alfa, B=B, 
                    method="lm_robust",
                    xlab="Hematócrito (%)", ylab="Hemoglobina (mg/dl)", 
                    bg="transparent", col=col_HB, pch=pch_HB)
# HT x HEM
reslmr_hem <- correg(Gestantes$HT, Gestantes$HEM,
                     alpha=alfa, B=B, 
                     method="lm_robust",
                     xlab="Hematócrito (%)", ylab="Hemácia (milhão/mm³)", 
                     bg="transparent", col=col_HEM, pch=pch_HEM)
# HT x LEUC
reslmr_leuc <- correg(Gestantes$HT, Gestantes$LEUC,
                      alpha=alfa, B=B, 
                      method="lm_robust",
                      xlab="Hematócrito (%)", ylab="Leucócitos (milhar/mm³)", 
                      bg="transparent", col=col_LEUC, pch=pch_LEUC)
