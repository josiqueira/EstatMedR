source("eiras.bartitle.R")

options(warn=-1) # disable warnings
data(iris)
result <- MVN::mvn(data = iris,
                   subset="Species",
                   # R=5e4,
                   # mvnTest = "energy",
                   mvnTest = "hz",
                   univariateTest = "SF",
                   showOutliers=TRUE)
print(result$multivariateNormality)
print(result$univariateNormality)
print(result$multivariateOutliers)

options(warn=0) # enable warnings
