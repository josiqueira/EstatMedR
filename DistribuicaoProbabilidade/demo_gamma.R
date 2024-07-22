library(DescTools)

source("eiras.numeric.summary.R")

valgamma <- rgamma(1e6, shape=2)
densgamma <- density(valgamma)
plot (densgamma, 
      main="Distribuição Gama", xlab="Valores", ylab="Densidade")
print(numeric.summary(valgamma))
skewgamma <- DescTools::Skew(valgamma)
cat("\nSkewness (método 3) = ",skewgamma,"\n", sep="")
