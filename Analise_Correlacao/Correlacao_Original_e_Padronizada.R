estatura <- c(172, 173, 174, 175, 176, 177, 178, 179, 180)
massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)

cat("\nValores originais:\n")
cat("estaturas:",estatura,"\n")
cat("massas corporais:",massa,"\n")
cat("\nCorrelação com as variáveis originais:\n")
correlacao <- cor.test(estatura, massa)
print(correlacao)

# padronizando x e y
z_estatura <- scale(estatura)
z_massa <- scale(massa)
cat("\nValores padronizados:\n")
cat("estatura padronizada:",round(z_estatura,3),"\n")
cat("massa corporal padronizada:",round(z_massa,3),"\n")
cat("\nCorrelação com as variáveis padronizadas:\n")
correlacao_padrao <- cor.test(z_estatura, z_massa)
print(correlacao_padrao)
