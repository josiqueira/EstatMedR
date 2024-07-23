library(heplots)
library(jmv)
library(rrcov)
library(ICSNP)
library(MANOVA.RM)
library(tidyr)
library(npmv)
library(rrcov)
library(SHT)
library(readxl)
library(ggplot2)
library(psych)
library(GGally)
library(car)
library(carData)
# Applied Multivariate Statistics with R - Zelterman - 2015
bivCI <- function(s, xbar, n, alpha, m)
  # returns m (x,y) coordinates of 1-alpha joint confidence ellipse of mean
{
  x <- sin(2 * pi * (0 : (m - 1)) / (m - 1)) # m points on a unit circle
  y <- cos(2 * pi * (0 : (m - 1)) / (m - 1))
  cv <- qchisq(1 - alpha, 2) # chi-squared critical value
  cv <- cv / n # value of quadratic form
  for (i in 1 : m)
  {
    pair <- c(x[i], y[i]) # ith (x,y) pair
    q <- pair %*% solve(s,pair) # quadratic form
    x[i] <- x[i] * sqrt(cv / q) + xbar[1]
    y[i] <- y[i] * sqrt(cv / q) + xbar[2]
  }
  cbind(x, y)
}

alfa <- 0.05
B <- 1e3

Dados <- na.omit(as.data.frame(readxl::read_excel("Adm2008.xlsx")))
Masc <- subset(Dados, Genero=="Masculino", select=c(Estatura, MCT))
Fem <- subset(Dados, Genero=="Feminino", select=c(Estatura, MCT))

# 1 condicao
psych::describeBy(Dados[,c("Estatura","MCT")], 
                  group=Dados$Genero, 
                  data=Dados)
cor(Masc)
cor(Fem)

cov.M <- cov(Masc)
n.M <- nrow(Masc)
k <- 2 # numero de medidas
H0.M <- c(1.75,75)
mean.M <- as.numeric(colMeans(Masc))
Sinv.M <- as.numeric(solve(cov.M))
F.M <- (n.M*(n.M-k)/((n.M-1)*k))*
     (mean.M-H0.M)%*%solve(cov.M)%*%(mean.M-H0.M)
p.M <- 1-pf(q=F.M,df1=k,df2=n.M-k)
cat("p (Masculino) = ", p.M, "\n", sep="")
sunflowerplot(Masc, col = "gray", pch = 16, cex.lab = 1)
lines(bivCI(s = cov.M, xbar = mean.M, n = dim(Masc)[1],
            alpha = alfa, m = 1000),
      type = "l", col = "black", lwd = 1)
lines(colMeans(Masc)[1], colMeans(Masc)[2], pch = 3, cex = .8,
      type = "p", lwd = 1)
abline(h=75, lty=2)
abline(v=1.75, lty=2)

n.F <- nrow(Fem)
cov.F <- cov(Fem)
H0.F <- c(1.65,55)
mean.F <- as.numeric(colMeans(Fem))
Sinv.F <- as.numeric(solve(cov.F))
F.F <- (n.F*(n.F-k)/((n.F-1)*k))*
  (mean.F-H0.F)%*%solve(cov.F)%*%(mean.F-H0.F)
p.F <- 1-pf(q=F.F,df1=k,df2=n.F-k)
cat("p (Feminino) = ", p.F, "\n", sep="")
sunflowerplot(Fem, col = "gray", pch = 16, cex.lab = 1)
lines(bivCI(s = cov.F, xbar = mean.F, n = dim(Fem)[1],
            alpha = alfa, m = 1000),
      type = "l", col = "black", lwd = 1)
lines(colMeans(Fem)[1], colMeans(Fem)[2], pch = 3, cex = .8,
      type = "p", lwd = 1)
abline(h=55, lty=2)
abline(v=1.65, lty=2)
# 2 condicoes independentes
Masci <- subset(Dados, Genero=="Masculino", select=c(Estatura, MCT, Idade))
Femi <- subset(Dados, Genero=="Feminino", select=c(Estatura, MCT, Idade))
jmv::corrPart(Masci, 
              vars=vars(Estatura,MCT), 
              controls=vars(Idade),
              n=TRUE,
              type="part")
jmv::corrPart(Femi, 
              vars=vars(Estatura,MCT), 
              controls=vars(Idade),
              n=TRUE,
              type="part")
jmv::corrPart(Masci, 
              vars=vars(Estatura,MCT), 
              controls=vars(Idade),
              n=TRUE,
              type="semi")
jmv::corrPart(Femi, 
              vars=vars(Estatura,MCT), 
              controls=vars(Idade),
              n=TRUE,
              type="semi")
heplots::covEllipses(Dados[,c("Estatura","MCT")], 
                     Dados$Genero, 
                     col=c("black", "black", "darkgray"),
                     fill=FALSE,
                     method="classical", 
                     add=FALSE)
heplots::covEllipses(Dados[,c("Estatura","MCT")], 
                     Dados$Genero, 
                     col=c("black", "black", "darkgray"),
                     fill=FALSE,
                     method="mcd", 
                     add=FALSE)
print(x <- heplots::boxM(Dados[,c("Estatura","MCT")], 
                         Dados$Genero))
plot(x)
car::scatterplotMatrix(Dados[,c("Estatura","MCT")],
                       groups=Dados$Genero, 
                       data=Dados,
                       regLine=FALSE, 
                       smooth=FALSE, 
                       boxplots=TRUE, 
                       by.groups=TRUE,
                       ellipse=list(levels=c(1-alfa), 
                                    robust=TRUE, 
                                    fill=FALSE),
                       grid=FALSE,
                       col=c("black","darkgray"), 
                       cex=0.5,
                       cex.labels=1,
                       row1attop=TRUE)
GGally::ggpairs(Dados[,c("Estatura","MCT","Genero")], 
                ggplot2::aes(colour=Genero))

SHT::mean2.1931Hotelling(as.matrix(Masc), as.matrix(Fem))
rrcov::T2.test(as.matrix(Dados[,c("Estatura","MCT")]) ~ Dados$Genero)
SHT::mean2.1965Yao(as.matrix(Masc), as.matrix(Fem))
SHT::mean2.1980Johansen(as.matrix(Masc), as.matrix(Fem)) 

summary(manova(cbind(Estatura, MCT) ~ Genero, data=Dados))

anova(fit <- lm(cbind(Estatura, MCT) ~ Genero, data=Dados))
car::Anova(fit,
           univariate=FALSE, multivariate=TRUE)
heplots::etasq(fit)

# MANCOVA, assuming equal slopes
jmv::mancova(data=Dados,
             deps=vars(Estatura, MCT),
             factors=c(Genero),
             covs=c(Idade),
             multivar="pillai")

rrcov::Wilks.test(x=Dados[,c("Estatura","MCT")],
                  grouping=Dados$Genero,
                  method="mcd",
                  nrep=B)

npmv::nonpartest(Estatura | MCT ~ Genero,
                 data=Dados,
                 permreps=B,
                 factors.and.variables=TRUE,
                 plot=FALSE)

summary(MANOVA.RM::MANOVA.wide(cbind(Estatura, MCT) ~ Genero, 
                               data=Dados, 
                               iter=B, 
                               alpha=alfa, 
                               resampling="WildBS", 
                               seed=123, 
                               nested.levels.unique=FALSE, 
                               dec=4))

npmv::ssnonpartest(Sepal.Length | Sepal.Width | Petal.Length | Petal.Width 
                   ~ Species, 
                   data=iris, 
                   alpha=alfa,
                   factors.and.variables=TRUE,
                   plot=FALSE,
                   releffects=FALSE)

# 3 condicoes independentes
Dados <- datasets::iris
setosa <- subset(Dados, 
               Species=="setosa", 
               select=c(Sepal.Length, Sepal.Width, 
                        Petal.Length, Petal.Width))
versicolor <- subset(Dados, 
                 Species=="versicolor", 
                 select=c(Sepal.Length, Sepal.Width, 
                          Petal.Length, Petal.Width))
virginica <- subset(Dados, 
                     Species=="virginica", 
                     select=c(Sepal.Length, Sepal.Width, 
                              Petal.Length, Petal.Width))

psych::describeBy(Dados[,c("Sepal.Length","Sepal.Width",
                           "Petal.Length","Petal.Width")], 
                  group=Dados$Species, 
                  data=Dados)
cov(setosa)
cov(versicolor)
cov(virginica)
cor(setosa)
cor(versicolor)
cor(virginica)
heplots::covEllipses(Dados[,c("Sepal.Length","Sepal.Width",
                              "Petal.Length","Petal.Width")], 
                     group=Dados$Species,  
                     col=c("black", "black", "black", "darkgray"),
                     fill=FALSE,
                     method="classical", 
                     add=FALSE)
heplots::covEllipses(Dados[,c("Sepal.Length","Sepal.Width",
                              "Petal.Length","Petal.Width")], 
                     group=Dados$Species,  
                     col=c("black", "black", "black", "darkgray"),
                     fill=FALSE,
                     method="mcd", 
                     add=FALSE)
print(x <- heplots::boxM(Dados[,c("Sepal.Length","Sepal.Width",
                                  "Petal.Length","Petal.Width")], 
                         group=Dados$Species))
plot(x)
car::scatterplotMatrix(Dados[,c("Sepal.Length","Sepal.Width",
                                "Petal.Length","Petal.Width")], 
                       group=Dados$Species, 
                       data=Dados,
                       regLine=FALSE, 
                       smooth=FALSE, 
                       boxplots=TRUE, 
                       by.groups=TRUE,
                       ellipse=list(levels=c(1-alfa), 
                                    robust=TRUE, 
                                    fill=FALSE),
                       grid=FALSE,
                       col=c("black","darkgray","gray"), 
                       cex=0.6,
                       cex.labels=0.8,
                       row1attop=TRUE)
GGally::ggpairs(Dados[,c("Sepal.Length","Sepal.Width",
                         "Petal.Length","Petal.Width",
                         "Species")], 
                ggplot2::aes(colour=Species))

summary(manova(cbind(Sepal.Length, Sepal.Width, 
                     Petal.Length, Petal.Width) ~ Species, 
        data=Dados))

anova(fit <- lm(cbind(Sepal.Length, Sepal.Width, 
                      Petal.Length, Petal.Width) ~ Species, 
                data=Dados))
car::Anova(fit, univariate=FALSE, multivariate=TRUE)
heplots::etasq(fit)

jmv::mancova(data=Dados,
             deps=vars(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
             factors=c(Species),
             multivar="pillai")

rrcov::Wilks.test(x=Dados[,c("Sepal.Length","Sepal.Width",
                             "Petal.Length","Petal.Width")],
                  grouping=Dados$Species,
                  method="mcd",
                  nrep=B)

summary(MANOVA.RM::MANOVA.wide(cbind(Sepal.Length, Sepal.Width, 
                                     Petal.Length, Petal.Width) ~ Species, 
                               data=Dados, 
                               iter=B, 
                               alpha=alfa, 
                               resampling="WildBS", 
                               seed=123, 
                               nested.levels.unique=FALSE, 
                               dec=4))

npmv::nonpartest(Sepal.Length | Sepal.Width | 
                 Petal.Length | Petal.Width ~ Species,
                 data=Dados,
                 permreps=B,
                 factors.and.variables=TRUE,
                 plot=FALSE,
                 releffects=FALSE)

# rmMANOVA: 2 condicoes dependentes (pre-pos): wide
Dados <- na.omit(as.data.frame(readxl::read_excel("Aracnofobia.xlsx",
                                                  sheet="Aracnofobia_wide")))
biv <- Dados[,c("QMA_dif","TAC_dif")]
cor(biv)
GGally::ggpairs(Dados[,c("QMA_dif","TAC_dif")])
summary(lm(cbind(QMA_dif,TAC_dif)~1,data=biv))
n <- nrow(biv)
cov <- cov(biv)
H0 <- c(0,0)
k <- length(H0)
mean <- as.numeric(colMeans(biv))
Sinv <- solve(cov)
F <- (n*(n-k)/((n-1)*k))*
  (mean-H0)%*%Sinv%*%(mean-H0)
p <- 1-pf(q=F,df1=k,df2=n-k)
cat("p = ", p, "\n", sep="")
sunflowerplot(biv, col = "gray", pch = 16, cex.lab = 1)
lines(bivCI(s = cov(biv), xbar = colMeans(biv), n = dim(biv)[1],
            alpha = alfa, m = 1000),
      type = "l", col = "black", lwd = 1)
lines(colMeans(biv)[1], colMeans(biv)[2], pch = 3, cex = .8,
      type = "p", lwd = 1)
abline(h=0, lty=2)
abline(v=0, lty=2)

# rmMANOVA: 2 condicoes depedentes (pre-pos): long
Dados <- na.omit(as.data.frame(readxl::read_excel("Aracnofobia.xlsx",
                                                  sheet="Aracnofobia_long")))

# rmMANOVA
Dados <- na.omit(as.data.frame(readxl::read_excel("NewDrug.xlsx")))

fit <- lm(cbind(resp, pulse) ~ id + drug*time, data=Dados)
car::Anova(fit)
heplots::etasq(fit)

fit.aov <- manova(cbind(resp, pulse) ~ id + drug*time, data=Dados)
summary(fit.aov)

fit.aovE <- manova(cbind(resp, pulse) ~  drug*time + Error(id), data=Dados)
summary(fit.aovE)

fit <- MANOVA.RM::multRM(cbind(resp, pulse) ~ drug*time, 
                         data=Dados, 
                         subject="id", 
                         within="time",
                         iter=B, 
                         alpha=alfa, 
                         seed=123, 
                         dec=4)
summary(fit)




