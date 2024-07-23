source("eiras.shape.test.R")

ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
alfa <- 0.05

cat("\nSem treino:",SemTreino)
cat("\n\tmedia =",ms <- mean(SemTreino))
cat("\n\td.p. =",ss <- sd(SemTreino))
cat("\n\tn =",ns <- length(SemTreino))
cat("\nCom treino:",ComTreino)
cat("\n\tmedia =",mc <- mean(ComTreino))
cat("\n\td.p. =",sc <- sd(ComTreino))
cat("\n\tn =",nc <- length(ComTreino))
cat("\n")
dif <- mean(ComTreino) - mean(SemTreino)
cat("\nDiferenca das medias amostrais (Com treino - Sem treino) = ",dif,sep="")
cat("\n")

# teste das premissas
s <- list()
s[["group1"]] <- SemTreino
s[["group2"]] <- ComTreino
shape <- shape.test(s, labels = c("Sem Treino", "Com Treino"))

# teste t de Student
print(t.test(ComTreino,SemTreino,var.equal = TRUE))

# graficos
x <- seq(0,7,length.out=100)
plot(density(SemTreino,na.rm=TRUE), xlim=c(0,7), ylim=c(0,0.6),
     main="Distribuicao, sem treino",xlab="Score")
lines(x,dnorm(x,ms,ss),lty=2)
text(0,0.5,pos=4,
     paste0("Simmetry: p=",
            round(as.numeric(shape[["simmetry.Sem Treino"]]$p.value),4),
            "\nNormality: p=",
            round(as.numeric(shape[["normality.Sem Treino"]]$p.value),4),
            "\nn=",length(SemTreino)
            )
     )

plot(density(ComTreino,na.rm=TRUE), xlim=c(0,7), ylim=c(0,0.6),
     main="Distribuicao, com treino",xlab="Score")
lines(x,dnorm(x,mc,sc),lty=2)
text(0,0.5,pos=4,
     paste0("Simmetry: p=",
            round(as.numeric(shape[["simmetry.Com Treino"]]$p.value),4),
            "\nNormality: p=",
            round(as.numeric(shape[["normality.Com Treino"]]$p.value),4),
            "\nn=",length(ComTreino)
           )
    )
