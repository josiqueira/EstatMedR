set.seed(4090)

horas.estudo <- runif(n=50, min=10, max=70)
nota.obtida <- 0*horas.estudo + 50
nota.obtida <- nota.obtida + runif(length(horas.estudo), min=-30, max=30)

plot(horas.estudo, nota.obtida,
     xlim=c(0,80),
     ylim=c(0,100),
     main=paste("Hipótese do estudante\n(independente de meu estudo, serei aprovado)",sep=""),
     xlab="Horas de estudo na semana",
     ylab="Nota obtida no exame"
)
