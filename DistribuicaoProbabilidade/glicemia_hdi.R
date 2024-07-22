library("HDInterval")

alfa <- 0.05

valores <- c(5.5, 5.2, 5.2, 5.8, 5.6, 4.6, 
              5.6, 5.9, 4.7, 5.0, 5.7, 5.2)

# HDI com outlier
densidade <- density(valores,na.rm=TRUE)
plot (densidade,
      main="Distribuição da Glicemia de Jejum", 
      xlab="Glicemia (mmol/l)", ylab="Densidade",
      lwd=2)
hdir1 <- HDInterval::hdi(densidade, credMass=1-alfa)
ht <- attr(hdir1, "height")
HDIll1 <- hdir1[1]
HDIul1 <- hdir1[2]
cat("Intervalo de predição:\n")
cat("  HDI95 = [",HDIll1,",",HDIul1,"]\n", sep="")
segments(HDIll1, ht, HDIul1, ht, lwd=3, col= "blue")
lines(rep(as.numeric(HDIll1),2),c(0,ht), lty=2, col= "blue")
lines(rep(as.numeric(HDIul1),2),c(0,ht), lty=2, col= "blue")
