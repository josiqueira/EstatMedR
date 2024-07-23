Dados <- readRDS("Gestante.rds")
Vars <- c("HT", "HB", "HEM")
print(summary(Dados[Vars]))
print(cor(Dados[Vars]), digits=2)
sunflowerplot(HT~HB, 
              xlab="Hematócrito (%)", ylab="Hemoglobina (mg/dl)",
              data=Dados)
lines(lowess(Dados$HT~Dados$HB), lty=2)
sunflowerplot(HT~HEM, 
              xlab="Hematócrito (%)", ylab="Hemácias (milhão/mm3)",
              data=Dados)
lines(lowess(Dados$HT~Dados$HEM), lty=2)
sunflowerplot(HT~LEUC, 
              xlab="Hematócrito (%)", ylab="Leucócitos (milhar/mm3)",
              data=Dados)
lines(lowess(Dados$HT~Dados$LEUC), lty=2)

