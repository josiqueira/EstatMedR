source("eiras.bartitle.R")

options(warn=-1) # disable warnings

print(res <- heplots::boxM(iris[,1:4], iris$Species))
plot(res, main="Iris")

options(warn=0) # enable warnings
