massa <- c(61, 73, 68, 74, 65, 82, 69, 81, 83)
d.massa <- density(massa)
plot(d.massa, main="Variável não padronizada", 
     xlab="Massa Corpórea (kg)",
     ylab="Densidade")
# padronizacao usando a função scale()
massa.z <- scale(massa)
d.massa.z <- density(massa.z)
plot(d.massa.z, main="Variável padronizada", 
     xlab="Massa Corpórea (z)",
     ylab="Densidade")
