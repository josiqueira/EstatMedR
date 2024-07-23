source("eiras.friendlycolor.R")
source("eiras.correg.R")

# Load the data
data("Boston", package = "MASS")

x <- Boston$lstat[Boston$medv<=20]
y <- Boston$medv[Boston$medv<=20]
alfa <- 0.05
B <- 0
lst <- correg(x, y,
              alpha=alfa, B=B, 
              method="lm_robust",
              xlab="BSS (%)", 
              ylab="MedianaCasa (x1000 USD)", 
              bg="transparent", col=friendlycolor(14), pch=21)
