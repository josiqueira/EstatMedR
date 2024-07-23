cat("Correlacao da Estatura com MCT:")
rXY <- 0.79 
cat(rXZ," (r^2=",rXY^2,")\n",sep="")
cat("Correlacao da Estatura com Genero:")
rXZ <- 0.63
cat(rXZ," (r^2=",rXZ^2,")\n",sep="")
cat("Correlacao da MCT com Genero:")
rYZ <- 0.70
cat(rYZ," (r^2=",rYZ^2,")\n",sep="")

cat("\nCorrelacao Parcial:")
r.Parcial.Pearson <- (rXY - rXZ*rYZ)/(sqrt(1 - rXZ^2)*sqrt(1 - rYZ^2))
cat(r.Parcial.Pearson," (r^2=",r.Parcial.Pearson^2,")\n",sep="") 

