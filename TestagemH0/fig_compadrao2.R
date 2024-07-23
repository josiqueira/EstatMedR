set.seed(4090)

horas.estudo <- runif(n=50, min=10, max=70)
nota.obtida <- 0.8*horas.estudo + 15
nota.obtida <- nota.obtida + runif(length(horas.estudo), min=-30, max=30)

plot(horas.estudo, nota.obtida,
     xlim=c(0,80),
     ylim=c(0,100),
     main=paste("Hipótese do professor\n(quanto mais o estudante dedica, melhor a nota obtida)",sep=""),
     xlab="Horas de estudo na semana",
     ylab="Nota obtida no exame"
)

df <- data.frame(horas.estudo, nota.obtida)
names(df) <- c("horas", "nota")

r <- which(df$horas>=25 & df$horas<=34)
points(df[r,], pch=21, col="black", bg="red")
reg <- lm(df$nota[r] ~ df$horas[r])
lines(df$horas[r], 
      reg$coefficients[1]+reg$coefficients[2]*df$horas[r],
      lwd=3)

r <- which(df$horas>=61)
points(df[r,], pch=21, col="black", bg="green")
reg <- lm(df$nota[r] ~ df$horas[r])
lines(df$horas[r], 
      reg$coefficients[1]+reg$coefficients[2]*df$horas[r],
      lwd=3)

r <- which(df$horas>=39 & df$horas<=58)
points(df[r,], pch=21, col="black", bg="blue")
reg <- lm(df$nota[r] ~ df$horas[r])
lines(df$horas[r], 
      reg$coefficients[1]+reg$coefficients[2]*df$horas[r],
      lwd=3)
