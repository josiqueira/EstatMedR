source("eiras.bartitle.R")

options(warn=-1) # disable warnings

Dados <- readRDS("Adm2008.rds")
Masc <- subset(Dados, Sexo=="Masculino", select=c(Estatura, MCT))

result <- MVN::mvn(data = Masc, 
                   # R=5e4,
                   # mvnTest = "energy",
                   mvnTest = "hz",
                   univariateTest = "SF",
                   showOutliers=TRUE)
cat(bartitle("Binormality test"))
print(result$multivariateNormality)
cat(bartitle("Normality test"))
print(result$univariateNormality)
cat(bartitle("Multivariate Outlier"))
print(result$multivariateOutliers)
options(warn=0) # enable warnings
