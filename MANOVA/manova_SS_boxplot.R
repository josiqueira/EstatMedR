set.seed(123) 
dados <- data.frame(
  fator1 = factor(rep(c("A", "B"), each = 30)),
  fator2 = factor(rep(c("X", "Y"), times = 30)),
  medida1 = rnorm(60, mean = 50, sd = 10),
  medida2 = rnorm(60, mean = 60, sd = 15),
  medida3 = rnorm(60, mean = 55, sd = 20)
)

modelo_manova <- manova(cbind(medida1, medida2, medida3) ~ fator1 * fator2, 
                        data = dados)

sumario <- summary(modelo_manova, intercept = FALSE)
sumario$SS


library(reshape2)  # for melt() function
library(ggplot2)
data(iris)
Iris.long <- melt(iris, 
                  id=c('Species'), 
                  measured=c('Sepal.Length', 'Sepal.Width',
                             'Petal.Length', 'Petal.Width'))
names(Iris.long) <- c('Group', 'Measure', 'Frequency')
IrisBoxplot <- ggplot2::ggplot(Iris.long, 
                               ggplot2::aes(Group, 
                                            Frequency, 
                                            color = Measure)) +
  ggplot2::geom_boxplot() + 
  ggplot2::labs(x='Species', 
                y='Size (cm)', 
                color='Measure') 
plot(IrisBoxplot)
rm(Iris.long)

setosa_vs_versicolor <- c(1, -1, 0)
contrasts(iris$Species) <- setosa_vs_versicolor
fit <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, 
              data = iris)
summary(fit)
