library (readxl)
source("eiras.tukey.try.R")

cc <- readxl::read_excel("CorpoCerebro.xlsx")

v.transformados <- tukey.try(cc$Cerebro, xlab="massa", show="all")

d.cerebro <- density(v.transformados[[2]])
txtun <- v.transformados[[1]]$equation
plot (d.cerebro, type="l", 
      main="Distribuição da massa do cérebro",
      xlab=paste("Tamanho do cérebro (",txtun,")",sep=""), 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(v.transformados[[2]], na.rm=TRUE),6),"\n", sep="")
cat("  dp = ", round(sd(v.transformados[[2]], na.rm=TRUE),6),"\n", sep="")
