estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
mean_estatura <- mean(estatura)
sd_estatura <- sd(estatura)
z_estatura <- (estatura-mean_estatura)/sd_estatura
mean_massa <- mean(massa)
sd_massa <- sd(massa)
z_massa <- (massa-mean_massa)/sd_massa
r <- cov(z_estatura,z_massa)
cat("r de Pearson = ",r,"\n", sep="")
