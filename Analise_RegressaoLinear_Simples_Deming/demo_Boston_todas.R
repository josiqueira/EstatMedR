source("eiras.correg.R")

# Load the data
data("Boston", package = "MASS")
alfa <- 0.05
B <- 0
lst <- correg(Boston$lstat, Boston$medv,
              alpha=alfa, B=B, 
              method="lm_robust",
              xlab="BSS (%)", 
              ylab="MedianaCasa (x1000 USD)", 
              bg="transparent", col=friendlycolor(14), pch=21)
