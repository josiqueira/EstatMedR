cat("\nTeste U de Mann-Whitney Convencional:\n")
print(wilcox.test(amostra1,amostra2, exact = FALSE))

# # Hollander & Wolfe, p. 110, results p. 111 and p. 126
# cat("\nTeste U de Mann-Whitney Exato:\n")
# datafrm <- data.frame(
#   valor = c(amostra1,amostra2),
#   grupo = factor(rep(c("amostra1", "amostra2"),
#                      c(length(amostra1), length(amostra2))))
# )
# print(coin::wilcox_test(valor ~ grupo, data = datafrm,
#                         distribution = "exact", conf.int = TRUE))
