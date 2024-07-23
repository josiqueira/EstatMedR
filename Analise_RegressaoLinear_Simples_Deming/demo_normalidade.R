Gestantes <- readRDS("Gestante.rds")

cat("\n\nTotal de Gestantes:",nrow(Gestantes))

cat("\n\nHT x HB:")
result <- MVN::mvn(data=Gestantes[,c("HT","HB")], 
                   mvnTest="hz", univariateTest="SW")
cat("\nTeste de normalidade bivariada:\n")
print(result$multivariateNormality)
cat("\nTeste de normalidade de cada variavel:\n")
print(result$univariateNormality)

cat("\n\nHT x HEM:")
result <- MVN::mvn(data=Gestantes[,c("HT","HEM")], 
                   mvnTest="hz", univariateTest="SW")
cat("\nTeste de normalidade bivariada:\n")
print(result$multivariateNormality)
cat("\nTeste de normalidade de cada variavel:\n")
print(result$univariateNormality)

cat("\n\nHT x LEUC:")
result <- MVN::mvn(data=Gestantes[,c("HT","LEUC")], 
                   mvnTest="hz", univariateTest="SW")
cat("\nTeste de normalidade bivariada:\n")
print(result$multivariateNormality)
cat("\nTeste de normalidade de cada variavel:\n")
print(result$univariateNormality)
