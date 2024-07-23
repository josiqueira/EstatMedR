options(warn=-1) # disable warnings

source("eiras.friendlycolor.R")
source("eiras.correg.R")
source("eiras.bivCI.R")
source("eiras.showdataframe.R")
source("eiras.numeric.summary.R")
source("eiras.bartitle.R")

colM <- friendlycolor(7) # azul
colF <- friendlycolor(30) # vermelho
pchM <- 24
pchF <- 21

alpha <- 0.05

cat(bartitle("Data"))
Dados <- readRDS("Adm2008.rds")
Dados <- Dados[, c("Sexo","Estatura","MCT")]
showdataframe(Dados, head=5, tail=3)

cat(bartitle("Summary"))

cat(bartitle("Masculino",2))
cat(bartitle("Estatura",3))
print(numeric.summary(Dados$Estatura[Dados$Sexo=="Masculino"]))
cat(bartitle("MCT",3))
print(numeric.summary(Dados$MCT[Dados$Sexo=="Masculino"]))

dt_tmp <- data.frame(Dados$Estatura[Dados$Sexo=="Masculino"],
                     Dados$MCT[Dados$Sexo=="Masculino"])
names(dt_tmp) <- c("Estatura","MCT")
cat(bartitle("Correlation matrix",3))
print(cor(dt_tmp), digits=2)
cat(bartitle("Covariance matrix",3))
print(cov(dt_tmp), digits=2)

cat(bartitle("Feminino",2))
cat(bartitle("Estatura",3))
print(numeric.summary(Dados$Estatura[Dados$Sexo=="Feminino"]))
cat(bartitle("MCT",3))
print(numeric.summary(Dados$MCT[Dados$Sexo=="Feminino"]))

dt_tmp <- data.frame(Dados$Estatura[Dados$Sexo=="Feminino"],
                     Dados$MCT[Dados$Sexo=="Feminino"])
names(dt_tmp) <- c("Estatura","MCT")
cat(bartitle("Correlation matrix",3))
print(cor(dt_tmp), digits=2)
cat(bartitle("Covariance matrix",3))
print(cov(dt_tmp), digits=2)

print(psych::describeBy(Dados[,2:3], g = Dados$Sexo,
                        mat=1, digits = 2))
Dados.long <- reshape2::melt(Dados, 
                            id=c("Sexo"), 
                            measured=c("Estatura","MCT"))
names(Dados.long) <- c('Group', 'Measure', 'Value')
Dados.long.Boxplot <- ggplot2::ggplot(Dados.long, 
                               ggplot2::aes(Group, 
                                            Value, 
                                            color = Measure)) +
  ggplot2::geom_boxplot() + 
  ggplot2::labs(x="Sexo", 
                y="Valor", 
                color="Medida") 
plot(Dados.long.Boxplot)
rm(Dados.long)

print(GGally::ggpairs(data = Dados, mapping = ggplot2::aes(color = Sexo,
                                                                fill = Sexo,
                                                                alpha = .5),
                      progress = FALSE))

correg(Dados$Estatura[Dados$Sexo=="Masculino"], 
       Dados$MCT[Dados$Sexo=="Masculino"],
       method="raw", conf.band = FALSE, lowess=FALSE,
       alpha=0.05, B=0, 
       xlab="Estatura (cm)", ylab="MCT (kg)", 
       main="Elipses de confiança de 95% Bonferroni\nEstudante de Administração", 
       col=colM, bg="transparent", pch=pchM,
       show.equation = FALSE, suppress.text = TRUE)
correg(Dados$Estatura[Dados$Sexo=="Feminino"], 
       Dados$MCT[Dados$Sexo=="Feminino"],
       method="raw", conf.band = FALSE, lowess=FALSE,
       alpha=0.05, B=0, 
       xlab="Estatura (cm)", ylab="MCT (kg)", 
       col=colF, bg="transparent", pch=pchF,
       show.equation = FALSE, suppress.text = TRUE, add=TRUE)

# elipses de confianca do centroide populacional com correçao de Bonferroni
df_tmp <- data.frame(Dados$Estatura[Dados$Sexo=="Masculino"], 
                     Dados$MCT[Dados$Sexo=="Masculino"])
names(df_tmp) <- c("Estatura","MCT")
df_tmp <- df_tmp[!is.na(df_tmp$Estatura),]
df_tmp <- df_tmp[!is.na(df_tmp$MCT),]
lines(bivCI(s = var(df_tmp), 
            xbar = colMeans(df_tmp), 
            n = nrow(df_tmp),
            alpha = alpha/2, m = 10000),
      type = "l", col = colM, lwd = 2)
cx <- mean(Dados$Estatura[Dados$Sexo=="Masculino"], na.rm=TRUE)
cy <- mean(Dados$MCT[Dados$Sexo=="Masculino"], na.rm=TRUE)
points(cx,cy,pch=21,col=colM,bg=colM)
text(cx,cy, pos=2, "M", col=colM)

df_tmp <- data.frame(Dados$Estatura[Dados$Sexo=="Feminino"], 
                     Dados$MCT[Dados$Sexo=="Feminino"])
names(df_tmp) <- c("Estatura","MCT")
df_tmp <- df_tmp[!is.na(df_tmp$Estatura),]
df_tmp <- df_tmp[!is.na(df_tmp$MCT),]
lines(bivCI(s = var(df_tmp), 
            xbar = colMeans(df_tmp), 
            n = nrow(df_tmp),
            alpha = alpha/2, m = 10000),
      type = "l", col = colF, lwd = 2)
cx <- mean(Dados$Estatura[Dados$Sexo=="Feminino"], na.rm=TRUE)
cy <- mean(Dados$MCT[Dados$Sexo=="Feminino"], na.rm=TRUE)
points(cx,cy,pch=21,col=colF,bg=colF)
text(cx,cy, pos=2, "F", col=colF)

options(warn=0) # enable warnings
