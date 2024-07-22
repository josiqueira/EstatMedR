library (readxl)
cc <- readxl::read_excel("CorpoCerebro.xlsx")

v.padrao <- scale(cc$Cerebro, center = TRUE, scale = TRUE)
d.cerebro <- density(v.padrao)
txtun <- "escore-z"
plot (d.cerebro, type="l", 
      main="Distribuição da massa do cérebro",
      xlab=paste("Massa (",txtun,")",sep=""), 
      ylab="densidade", 
      lty=1, lwd=2)
cat("Verificação:\n")
cat("  média = ", round(mean(v.padrao, na.rm=TRUE),6),"\n", sep="")
cat("  dp = ", round(sd(v.padrao, na.rm=TRUE),6),"\n", sep="")
