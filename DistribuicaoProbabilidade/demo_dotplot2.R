set.seed(123)
Estatura.m <- as.integer(rnorm(n=50, mean=177, sd=10))
Estatura.f <- as.integer(rnorm(n=50, mean=165, sd=8))
stripchart(Estatura.m, method="stack", 
           offset=0.5, at=0.15, 
           pch=4, ylab="Contagem", xlab ="Estatura (cm)")
stripchart(Estatura.f, add=TRUE, method="stack", 
           offset=0.5, at=0.15, pch=1)
legend("topright",c("Masculina", "Feminina"),
       pch=c(4,1))