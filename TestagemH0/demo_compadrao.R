# s <- round(runif(1,1,10000),0)
# print(s)
# set.seed(s)
# set.seed(576)
set.seed(4090)

n <- 15

horas.estudo <- runif(n=150, min=10, max=70)
nota.obtida <- 0.8*horas.estudo + 15
nota.obtida <- nota.obtida + runif(length(horas.estudo), min=-30, max=30)

plot(horas.estudo, nota.obtida,
     xlim=c(0,80),
     ylim=c(0,100),
     main=paste("Caso o professor esteja certo\n(quanto mais dedica, melhor a nota)",sep=""),
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

rm(.Random.seed, envir=globalenv())
sucesso <- falha <- 0
for (i in 1:1000)
{
  r <- sample(1:nrow(df),size=n,replace=FALSE)
  reg <- lm(df$nota[r] ~ df$horas[r])
  p <- summary(reg)$coefficients[2,4]
  if (reg$coefficients[2] > 0.4) 
  {
    sucesso <- sucesso+1
    tempo <- 0.2
    col <- "green"
  } else 
  {
    falha <- falha+1
    tempo <- 1
    col <- "red"
  }
  plot(horas.estudo, nota.obtida,
       xlim=c(0,80),
       ylim=c(0,100),
       main=paste("Caso o professor esteja certo\n",
                  "(quanto mais dedica, melhor a nota)\n",
                  "sem padrao ", falha," x ",
                  sucesso," com padrao",
                  " (",round((sucesso/(sucesso+falha))*100,2),"%)",
                  sep=""),
       xlab="Horas de estudo na semana",
       ylab="Nota obtida no exame"
  )
  points(df[r,], pch=21, col="black", bg=col)
  lines(df$horas[r], 
        reg$coefficients[1]+reg$coefficients[2]*df$horas[r],
        lwd=3)
  Sys.sleep(tempo)
  
}

