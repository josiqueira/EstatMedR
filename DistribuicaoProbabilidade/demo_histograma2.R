set.seed(123)
Estatura.m <- as.integer(rnorm(n=50, mean=177, sd=10))
Estatura.f <- as.integer(rnorm(n=50, mean=165, sd=8))
hist(Estatura.f, ylab="Contagem", xlab ="Estatura (cm)", 
     main="Estaturas dos adultos",
     col="#88000044")
hist(Estatura.m,  add=TRUE, col="#00008844")
legend("topleft",
       c("Feminina","Masculina"),
       pt.bg=c("#88000044","#00008844"),
       pch=22
       )