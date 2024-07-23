source("eiras.bartitle.R")

options(warn=-1) # disable warnings

cat(bartitle("MANOVA robusta"))
mnv <- rrcov::Wilks.test(x=iris[,1:4],
                          grouping=iris[,"Species"],
                          method="mcd",nrep=1e3)
print(mnv)
data(iris)
result <- MVN::mvn(data = iris,
                   subset="Species",
                   # R=5e4,
                   # mvnTest = "energy",
                   mvnTest = "hz",
                   univariateTest = "SF")
print(result$multivariateNormality)
print(result$univariateNormality)

print(res <- heplots::boxM(iris[,1:4], iris[,"Species"]))
plot(res, main="Iris")

# Robust One-way MANOVA (Bartlett Chi2) (heterocedástica e não-multinormal)
mnv <- rrcov::Wilks.test(x=iris[,1:4],
                         grouping=iris[,"Species"],
                         method="mcd",nrep=1e3)
print(mnv)


