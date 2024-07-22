# Intervalo de confiança de 95% da média populacional
suppressMessages(library(sandwich, warn.conflicts = FALSE))
suppressMessages(library(car, warn.conflicts = FALSE))
library(RcmdrMisc)
suppressMessages(library(gplots, warn.conflicts = FALSE))

estatura <- c(176, 183, 173, 191, 157, 152, 174, 166)
genero <- factor(c("M", "M", "M", "M", "F", "F", "F", "F"))
tabela <- data.frame(estatura, genero)
with(tabela, RcmdrMisc::plotMeans(estatura, genero, connect=FALSE,
                                  xlab="Genero", ylab="Estatura", main="IC95%"))  
with(tabela, gplots::plotmeans(estatura ~ genero, connect=FALSE,
                               xlab="Genero", ylab="Estatura", main="IC95%",
                               barcol="black"))