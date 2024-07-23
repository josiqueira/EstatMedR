source("eiras.shape.test.R")

options(warn=-1)

Dados <- readxl::read_excel("Simpatia.xlsx")
print(Dados)
cat("\nmedia(Antes):",mean(Dados$Antes))
cat("\nmedia(Depois):",mean(Dados$Depois))
dif <- mean(Dados$Depois) - mean(Dados$Antes)
cat("\nDiferenca das medias (Depois-Antes) = ",dif,"\n",sep="")

difs <- Dados$Depois-Dados$Antes
s <- list()
s[["difs"]] <- difs
shape <- shape.test(s, labels = c("Diferencas"))

dir.create("image",showWarnings=FALSE)
fileimg <- file.path("image","densDifs_W.png")
png(fileimg)
v <- Dados$Depois - Dados$Antes
dens.v <- density(v)
plot(dens.v, main="", xlab="Diferencas (Depois - Antes)", ylab="Densidade")
dev.off()
cat(paste("\nGrafico guardado em ",fileimg,"\n", sep=""))

cat("\nTeste W de Wilcoxon Convencional:\n")
print(with(Dados, wilcox.test(Depois, Antes, 
                              mu=0,
                              paired=TRUE, 
                              correct=FALSE,
                              conf.int=TRUE,
                              conf.level=0.95,
                              exact=FALSE)))

# Hollander & Wolfe, p. 39, results p. 40 and p. 53
cat("\nTeste W de Wilcoxon Exato:\n")
print(with(Dados, exactRankTests::wilcox.exact(Depois, Antes, 
                                               paired=TRUE, 
                                               conf.int=TRUE,
                                               conf.level=0.95)))

options(warn=0)
