source("eiras.showdataframe.R")

Dados <- readRDS("Felicidade.rds")
showdataframe(Dados, head=4, tail=3)

# todas as colunas tratadas como intervalares
Dados[,] <- lapply(Dados[,],as.numeric)

# Alterando tipos de variável 
Dados$Q1 <- as.numeric(Dados$Q1) # variavel intervalar
Dados$Q2 <- as.ordered(Dados$Q2) # variavel ordinal
Dados$Q3 <- cut(Dados$Q3,breaks=c(0,2,5),labels=c(0,1))
Dados$Q3 <- ifelse(Dados$Q3=="0", Dados$Q3<-0, Dados$Q3<-1)
Dados$Q3 <- as.factor(Dados$Q3)
Dados$Q4 <- cut(Dados$Q4,breaks=c(0,2,5),labels=c(0,1))
Dados$Q4 <- ifelse(Dados$Q4=="0", Dados$Q4<-0, Dados$Q4<-1)
Dados$Q4 <- as.factor(Dados$Q4)
Dados$Q5 <- cut(Dados$Q5,breaks=c(0,2,5),labels=c(0,1))
Dados$Q5 <- ifelse(Dados$Q5=="0", Dados$Q5<-0, Dados$Q5<-1)
Dados$Q5 <- as.ordered(Dados$Q5)
Dados$Q6 <- cut(Dados$Q6,breaks=c(0,2,5),labels=c(0,1))
Dados$Q6 <- ifelse(Dados$Q6=="0", Dados$Q6<-0, Dados$Q6<-1)
Dados$Q6 <- as.ordered(Dados$Q6)
Dados$Q7 <- cut(Dados$Q7,breaks=c(0,2,5),labels=c(0,1))
Dados$Q7 <- ifelse(Dados$Q7=="0", Dados$Q7<-0, Dados$Q7<-1)
Dados$Q7 <- as.numeric(Dados$Q7)
Dados$Q8 <- cut(Dados$Q8,breaks=c(0,2,5),labels=c(0,1))
Dados$Q8 <- ifelse(Dados$Q8=="0", Dados$Q8<-0, Dados$Q8<-1)
Dados$Q8 <- as.numeric(Dados$Q8)
Dados$Q9 <- cut(Dados$Q9,breaks=c(0,2,5),labels=c(0,1))
Dados$Q9 <- ifelse(Dados$Q9=="0", Dados$Q9<-0, Dados$Q9<-1)
Dados$Q9 <- as.logical(Dados$Q9)
Dados$Q10 <- cut(Dados$Q10,breaks=c(0,2,5),labels=c(0,1))
Dados$Q10 <- ifelse(Dados$Q10=="0", Dados$Q10<-0, Dados$Q10<-1)
Dados$Q10 <- as.logical(Dados$Q10)
# confere os tipos
print(sapply(Dados,class))
showdataframe(Dados, head=4, tail=3)

# matriz de correlacoes
print(polycor::hetcor(Dados, use="pairwise.complete.obs"), digits=1)
