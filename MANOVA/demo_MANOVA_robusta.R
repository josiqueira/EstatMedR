source("eiras.bartitle.R")

options(warn=-1) # disable warnings

cat(bartitle("Data"))
Dados <- readRDS("Adm2008.rds")
Dados <- Dados[, c("Sexo","Estatura","MCT")]

result <- MVN::mvn(data = Dados,
                   subset="Sexo",
                   # R=5e4,
                   # mvnTest = "energy",
                   mvnTest = "hz",
                   univariateTest = "SF",
                   showOutliers=TRUE)
print(result$multivariateNormality)
print(result$univariateNormality)
print(result$multivariateOutliers)

print(res <- heplots::boxM(Dados[,2:3], Dados[,1]))
plot(res, main="Adm2008")

# Robust One-way MANOVA (heterocedástica)
Dados.Mm <- as.matrix(subset(Dados, Sexo=="Masculino", select=c(Estatura, MCT)))
Dados.Fm <- as.matrix(subset(Dados, Sexo=="Feminino", select=c(Estatura, MCT)))
colMeans(Dados.Mm)
colMeans(Dados.Fm)
SHT::mean2.1980Johansen(Dados.Mm, Dados.Fm)

# Robust One-way MANOVA (Bartlett Chi2) (heterocedástica e não-multinormal)
mnv <- rrcov::Wilks.test(x=Dados[,c("Estatura","MCT")],
                         grouping=Dados[,"Sexo"],
                         method="mcd",nrep=1e3)
print(mnv)


