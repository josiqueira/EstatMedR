options(warn=-1) # disable warnings

source("eiras.friendlycolor.R")
source("eiras.correg.R")
source("eiras.bivCI.R")
source("eiras.showdataframe.R")
source("eiras.numeric.summary.R")
source("eiras.bartitle.R")

H0.M <- c(172,76.68)
alfa <- 0.05
B <- 1e3

colM <- friendlycolor(7) # azul
pchM <- 24
alpha <- 0.05

cat(bartitle("Data"))
Dados <- readRDS("Adm2008.rds")
Masc <- subset(Dados, Sexo=="Masculino", select=c(Estatura, MCT))
showdataframe(Masc, head=5, tail=3)
print(summary(Masc))

cat(bartitle("Summary"))

print(psych::describe(Masc[,c("Estatura","MCT")], data=Masc))
cat(bartitle("Correlation matrix",3))
print(cor(Masc), digits=2)
cat(bartitle("Covariance matrix",3))
print(cov(Masc), digits=2)

correg(Masc$Estatura, 
       Masc$MCT,
       method="raw", conf.band = FALSE, lowess=FALSE,
       alpha=0.05, B=0, 
       xlab="Estatura (cm)", ylab="MCT (kg)", 
       main="Estudante de Administração Masculino",
       col=colM, bg="transparent", pch=pchM,
       show.equation = FALSE, suppress.text = TRUE)

# elipses de confianca do centroide populacional
lines(bivCI(s = var(Masc), 
            xbar = colMeans(Masc), 
            n = nrow(Masc),
            alpha = alpha, m = 1e4),
      type = "l", col = colM, lwd = 2)
cx <- mean(Dados$Estatura[Dados$Sexo=="Masculino"], na.rm=TRUE)
cy <- mean(Dados$MCT[Dados$Sexo=="Masculino"], na.rm=TRUE)
points(cx,cy,pch=21,col=colM,bg=colM)
points(H0.M[1],H0.M[2],pch=10)
text(H0.M[1],H0.M[2],expression(H[0]),pos=2)
text(cx,cy, pos=3, "M", col=colM)

options(warn=0) # enable warnings

