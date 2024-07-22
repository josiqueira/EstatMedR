library(DescTools)
library(readxl)
source("eiras.numeric.summary.R")
source("eiras.friendlycolor.R")
set.seed(9428)

biometria <- readxl::read_excel("Biometria.xls")
biometria.homens <- biometria[biometria$sexo=="M",]
estaturas <- biometria.homens$estatura
minx <- min(estaturas, na.rm=TRUE)
maxx <- max(estaturas, na.rm=TRUE)
densidade.estatura <- density(estaturas, na.rm=TRUE)
maxy <- max(densidade.estatura$y, na.rm=TRUE)

media.med <- mean(estaturas, na.rm=TRUE)
desvpad.med <- sd(estaturas, na.rm=TRUE)
valnorm <- rnorm(1e6, mean=media.med, sd=desvpad.med)
densidade <- density(valnorm)
plot (densidade,
      main="Distribuição Normal", 
      xlab="Estatura (cm)", ylab="Densidade",
      xlim=c(minx,maxx), 
      ylim=c(0,maxy), 
      lwd=2)
lines(densidade.estatura, col=friendlycolor(7), lty=2, lwd=2)
abline(v=media.med, lty=2) # media
# desvio-padrao
lines(
   c(media.med,media.med+desvpad.med),
   dnorm(rep(media.med+desvpad.med,2), mean=media.med, sd=desvpad.med),
   lty=2)
cat("Sumário:\n")
sumario <- numeric.summary(valnorm)
print(sumario)
cat("\n")
media <- mean(valnorm)
cat("Média = ",media,"\n", sep="")
desvpad <- sd(valnorm)
cat("Desvio-padrão = ",desvpad,"\n", sep="")
variancia <- var(valnorm)
cat("Variância = ",variancia,"\n", sep="")
skewness <- DescTools::Skew(valnorm)
cat("Assimetria = ",skewness,"\n", sep="")
curtose <- DescTools::Kurt(valnorm)
cat("Ex.curtose = ",curtose,"\n", sep="")
