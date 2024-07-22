source ("eiras.numeric.summary.R")

glicemia <- c(5.5, 5.2, 5.2, 5.8, 5.6, 4.6, 
              5.6, 5.9, 4.7, 5.0, 5.7, 5.2)

n <- length(glicemia)
media <- mean(glicemia)
desvpad <- sd(glicemia)
qt <- qt(0.975, df=length(glicemia)-1)
lowerlimit <- media - qt * desvpad
upperlimit <- media + qt * desvpad

densidade <- density(glicemia)
skewness <- DescTools::Skew(glicemia)
curtose <- DescTools::Kurt(glicemia)
norm.x <- seq(from = min(densidade$x), to = max(densidade$x), length.out = 500)
norm.y <- dnorm(norm.x, mean=media, sd=desvpad)

ht <- dnorm(lowerlimit, mean=media, sd=desvpad)

plot (densidade,
      main="Distribuição da Glicemia de Jejum", 
      xlab="Glicemia (mmol/l)", ylab="Densidade",
      ylim=c(0,max(densidade$y,norm.y)), lwd=3)
lines(norm.x, norm.y, col="red", lty=2, lwd=2)
segments(lowerlimit, ht, upperlimit, ht, lwd=3, col= "brown")
lines(rep(as.numeric(lowerlimit),2),c(0,ht), lty=2, col= "brown")
lines(rep(as.numeric(upperlimit),2),c(0,ht), lty=2, col= "brown")
points(media,ht,pch=21,col="brown",bg="brown")

legend("topright", 
       c("Glicemia", "Normal"), 
       col=c("black", "red"),
       lwd=c(3,2), 
       lty=c(1,2), 
       box.lwd=0, bg="transparent")  

cat("Glicemia de jejum:\n")
cat("Sumário:\n")
sumario <- numeric.summary(glicemia)
print(sumario)
cat("Média = ",media,"\n", sep="")
cat("Desvio-padrão = ",desvpad,"\n", sep="")
cat("Assimetria = ",skewness,"\n", sep="")
cat("Ex.curtose = ",curtose,"\n", sep="")
cat("\nIntervalo de predição:\n")
cat("  IP95=[",lowerlimit,",",upperlimit,"]\n",sep="")
