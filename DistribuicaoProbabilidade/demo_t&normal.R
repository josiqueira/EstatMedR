source ("eiras.numeric.summary.R")

valores <- rt(1e6, df=5, ncp=0)
densidade <- density(valores)
media <- mean(valores)
desvpad <- sd(valores)
skewness <- DescTools::Skew(valores)
curtose <- DescTools::Kurt(valores)
norm.x <- seq(from = min(densidade$x), to = max(densidade$x), length.out = 500)
norm.y <- dnorm(norm.x, mean=media, sd=desvpad)
plot (densidade,
      main="Distribuição leptocúrtica", xlab="Valores", ylab="Densidade",
      xlim=c(-4,4),
      ylim=c(0,max(densidade$y,norm.y)), lwd=3)
lines(norm.x, norm.y, col="red", lty=2, lwd=2)
legend("topright", 
       c("t(5)", "Normal"), 
       col=c("black", "red"),
       lwd=c(3,2), 
       lty=c(1,2), 
       box.lwd=0, bg="transparent")  

cat("Sumário:\n")
sumario <- numeric.summary(valores)
print(sumario)
cat("\n")
cat("Média = ",media,"\n", sep="")
cat("Desvio-padrão = ",desvpad,"\n", sep="")
cat("Assimetria = ",skewness,"\n", sep="")
cat("Ex.curtose = ",curtose,"\n", sep="")
