source("eiras.bartitle.R")

options(warn=-1) # disable warnings

Dados <- readRDS("Adm2008.rds")
Dados <- Dados[, c("Sexo","Estatura","MCT")]

result <- MVN::mvn(data = Dados,
                   subset="Sexo",
                   # R=5e4,
                   # mvnTest = "energy",
                   mvnTest = "hz",
                   univariateTest = "SF")
print(result$multivariateNormality)
print(result$univariateNormality)
options(warn=0) # enable warnings
