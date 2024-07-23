# Correlacao_r_de_Pearson.R

# valores 
estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
correlacao <- cor.test(estatura, massa)
print(correlacao)
