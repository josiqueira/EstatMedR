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
showdataframe(iris, head=5, tail=3)

cat(bartitle("Summary"))
print(
psych::describeBy(iris[,c("Sepal.Length","Sepal.Width",
                           "Petal.Length","Petal.Width")], 
                  group=iris$Species,
                  mat=1,
                  digits=2)
)

cat(bartitle("Correlation & covariance"))
species <- unique(iris$Species)
colors <- c("#666666","#888888","#cccccc")
for (s in species)
{
  cat(bartitle(s,2))
  cat(bartitle("Correlation matrix",3))
  print(cor(subset(iris,Species=s,select=-Species)), digits=2)
  cat(bartitle("Covariance matrix",3))
  print(cov(subset(iris,Species=s,select=-Species)), digits=2)
}

Dados.long <- reshape2::melt(iris, 
                             id=c("Species"), 
                             measured=c("Sepal.Length",
                                        "Sepal.Width",
                                        "Petal.Length",
                                        "Petal.Width"))
names(Dados.long) <- c('Group', 'Measure', 'Value')
Dados.long.Boxplot <- ggplot2::ggplot(Dados.long, 
                                      ggplot2::aes(Group, 
                                                   Value, 
                                                   color = Measure)) +
  ggplot2::geom_boxplot() + 
  ggplot2::labs(x="Species", 
                y="Valor (cm)", 
                color="Medida") 
plot(Dados.long.Boxplot)
rm(Dados.long)

print(GGally::ggpairs(data = iris, mapping = ggplot2::aes(color = Species,
                                                           fill = Species,
                                                           alpha = .5),
                      progress = FALSE))

# grafico
car::scatterplotMatrix(iris[,c("Sepal.Length",
                               "Sepal.Width",
                               "Petal.Length",
                               "Petal.Width")], 
                       groups=iris$Species,
                       regLine=FALSE, 
                       smooth=FALSE, 
                       boxplots=TRUE, 
                       by.groups=TRUE,
                       ellipse=list(levels=c(0.95), 
                                    robust=TRUE, 
                                    fill=FALSE),
                       grid=FALSE,
                       col=colors, 
                       cex=0.5,
                       cex.lab=1)

# elipses de confianca do centroide populacional

for (i1 in 1:3)
{
  for (i2 in (i1+1):4)
  {
    dt_tmp <- iris[,c(i1,i2,5)]
    dt_tmp <- na.omit(dt_tmp)
    n1 <- names(dt_tmp)[1]
    n2 <- names(dt_tmp)[2]
    car::scatterplot(dt_tmp[,n1], dt_tmp[,n2], 
                     xlab=n1, ylab=n2,
                     group=dt_tmp[,3], 
                     regLine=list(lwd=0.5),
                     ellipse=FALSE,
                     smooth=FALSE,
                     grid=FALSE,
                     col=colors, 
                     legend=TRUE,
                     data=dt_tmp)
    for (s in 1:length(species))
    {
      dt_tmp2 <- dt_tmp[dt_tmp[3]==as.character(species[s]),c(1,2)]
      lines(bivCI(s = var(dt_tmp2), 
                  xbar = colMeans(dt_tmp2), 
                  n = nrow(dt_tmp2),
                  alpha = alpha/3, m = 10000),
            type = "l", col = colors[s], lwd = 2)
      cx <- mean(dt_tmp2[,1], na.rm=TRUE)
      cy <- mean(dt_tmp2[,2], na.rm=TRUE)
      points(cx,cy,pch=21,col=colors[s],bg=colors[s])
    }
  }
}

options(warn=0) # enable warnings
