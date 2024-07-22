library (readxl)
cc <- readxl::read_excel("CorpoCerebro.xlsx")

d.cerebro <- density(cc$Cerebro)
txtun <- "g"
plot (d.cerebro, type="l", 
      main="Distribuição da massa do cérebro",
      xlab=paste("Massa (",txtun,")",sep=""), 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(cc$Cerebro, na.rm=TRUE),6),"\n", sep="")
cat("  dp = ", round(sd(cc$Cerebro, na.rm=TRUE),6),"\n", sep="")
