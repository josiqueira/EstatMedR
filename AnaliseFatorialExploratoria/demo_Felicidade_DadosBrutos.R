source("eiras.showdataframe.R")
source("eiras.bartitle.R")

cat(bartitle("Data"))
Dados_brutos <- readRDS("Felicidade.rds")
showdataframe(Dados_brutos, head=4, tail=3)

cat(bartitle("EFA"))
num.factors <- 2
fa.fit <- psych::fa(r=Dados_brutos,
                    scores="regression",
                    nfactors=num.factors)
print(fa.fit, sort=TRUE,digits=2,cut=.3)

cat(bartitle("Scores"))
Dados_brutos$MR1 <- fa.fit$scores[,1]
Dados_brutos$MR2 <- fa.fit$scores[,2]
showdataframe(Dados_brutos, head=4, tail=3)


