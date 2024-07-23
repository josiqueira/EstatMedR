mu <- 173
sigma <- 7
alfa <- 0.05

set.seed(3)
estatura <- rnorm(mean=176, sd=7, n=100)
n <- length(estatura)

# estatistica
cat("Amostra:")
media <- mean(estatura)
cat("\n\tmédia = ",media, sep="")
ep <- sigma / sqrt(n)
cat("\n\tep = ",ep, sep="")
z25 <- abs(qnorm(p=alfa/2, mean=0, sd=1)) # |z| em 2.5%
IC95 = round(c(media-z25*ep, media+z25*ep),2)
cat("\n\tIC95 = [",IC95[1],",",IC95[2],"]", sep="")
