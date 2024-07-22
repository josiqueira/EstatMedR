library(DescTools)

source("eiras.numeric.summary.R")

valbeta <- rbeta(1e6, shape1=1, shape2=0.5)
densbeta <- density(valbeta)
plot (densbeta, 
      main="Distribuição Beta", xlab="Valores", ylab="Densidade")
print(numeric.summary(valbeta))
skewbeta <- DescTools::Skew(valbeta)
cat("\nSkewness (método 3) = ",skewbeta,"\n", sep="")
