Dados <- readRDS("WHOQOL.rds")

Dados[,4:27] <- lapply(Dados[,4:27],as.numeric)
res.pv <- pvclust::pvclust(Dados[4:27], 
                           method.dist="abscor",
                           method.hclust="average", 
                           nboot = 5e2)
plot(res.pv)
